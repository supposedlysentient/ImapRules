module Logging

open System
open System.IO
open System.Text
open MailKit
open MailKit.Net.Imap

let private makeDetector () =
    let assembly = typeof<ImapClient>.Assembly
    let typeName = "ImapAuthenticationSecretDetector"
    let detectorType =
        try
            assembly.GetTypes ()
            |> Array.filter (fun t -> t.Name = typeName)
            |> Array.head
        with _ ->
            failwith $"Failed to find type {typeName} in assembly {assembly.FullName}"
    let ctor = detectorType.GetConstructor [||]
    ctor.Invoke [||] :?> IAuthenticationSecretDetector

let Ansi = {|
    Black = "\x1b[30m"
    Red = "\x1b[91m"
    DarkRed = "\x1b[31m"
    Green = "\x1b[92m"
    DarkGreen = "\x1b[32m"
    Yellow = "\x1b[93m"
    DarkYellow = "\x1b[33m"
    Blue = "\x1b[94m"
    DarkBlue = "\x1b[34m"
    Magenta = "\x1b[95m"
    DarkMagenta = "\x1b[35m"
    Cyan = "\x1b[96m"
    DarkCyan = "\x1b[36m"
    DarkGrey = "\x1b[38;2;169;169;169m"
    White = "\x1b[97m"
    Reset = "\x1b[39m"
|}

type Stream = Config.Stream

let defaultVerbosity = Stream.Output

type ILogger =
    abstract Verbosity: Stream with get, set
    abstract member Log: Stream -> (obj -> unit)  // inheritance doesn't play nicely with curried args
    abstract member Log: obj -> unit
    abstract member LogError: Exception -> unit
    inherit IProtocolLogger
    inherit IDisposable

[<AbstractClass>]
type LoggerBase (logStream: IO.Stream, ?verbosity: Stream) =
    let mutable verbosity = defaultArg verbosity defaultVerbosity
    let mutable detector = makeDetector ()

    // TODO:
    //  - get rid of StreamWriter and use Stream directly
    //  - instantiate ProtocolLogger directly and share the stream
    //  - create CompoundStream for CompoundLogger
    // Reason: reduce risk of bugs in secret redaction
    do if logStream.CanSeek then logStream.Seek (0, SeekOrigin.End) |> ignore
    let writer = new StreamWriter (logStream)
    do writer.AutoFlush <- true

    member val Verbosity = verbosity with get, set

    member val FormatMessage: Stream -> string -> string = fun _ -> id with get, set

    abstract member Write: Stream -> (string -> unit)
    default this.Write (stream) =
        if stream <= this.Verbosity then writer.WriteLine else ignore

    abstract member Log: Stream -> (obj -> unit)
    default this.Log (stream: Stream) =
        fun (msg: obj) ->
            msg.ToString () |> this.FormatMessage stream |> this.Write stream

    abstract member Log: obj -> unit
    default this.Log (msg: obj) = this.Log Stream.Output msg

    abstract member LogError: Exception -> unit
    default this.LogError (e: Exception) = this.Log Stream.Error (e.ToString ())

    abstract member Dispose: unit -> unit
    default this.Dispose () =
        writer.Flush ()
        writer.Close ()
        logStream.Close ()
        logStream.Dispose ()

    interface ILogger with
        member this.Verbosity
            with get () = this.Verbosity
            and set (v) = this.Verbosity <- v
        member this.Log (stream: Stream) = this.Log stream
        member this.Log (msg: obj) = this.Log msg
        member this.LogError (e: Exception) = this.LogError e

    interface IProtocolLogger with
        member val AuthenticationSecretDetector = detector with get, set
        member this.LogConnect (uri: Uri) =
            this.Log Stream.Info $"Connected to {uri}"
        member this.LogClient (buffer: byte array, offset: int, count: int) =
            let msg = Encoding.ASCII.GetString (buffer, offset, count)
            this.Log Stream.Debug <| msg.TrimEnd ()
        member this.LogServer (buffer: byte array, offset: int, count: int) =
            let msg = Encoding.ASCII.GetString (buffer, offset, count)
            this.Log Stream.Debug <| msg.TrimEnd ()

    interface IDisposable with
        member this.Dispose () = this.Dispose ()

type NullLogger (?verbosity: Stream) =
    inherit LoggerBase (IO.Stream.Null, defaultArg verbosity defaultVerbosity)
    override this.Log (_: Stream) = fun (_: obj) -> ()
    override this.Log (_: obj) = ()
    override this.LogError (_: Exception) = ()
    override this.Dispose () = ()

type ConsoleLogger (?verbosity: Stream) as this =
    inherit LoggerBase (
        Console.OpenStandardOutput (),
        defaultArg verbosity defaultVerbosity)

    do this.FormatMessage <- (fun stream msg ->
        let color =
            match stream with
            | Stream.Error -> Ansi.Red
            | Stream.Warning -> Ansi.Magenta
            | Stream.Output -> Ansi.Green
            | Stream.Info -> Ansi.DarkGreen
            | Stream.Verbose -> Ansi.Cyan
            | Stream.Debug -> Ansi.Blue
            | s -> failwith $"Invalid stream: {s}" // appease the compiler, but have to work hard to get here!

        $"{color}{msg}{Ansi.Reset}")

    let errStream = Console.OpenStandardError ()
    let errWriter = new StreamWriter (errStream)
    do errWriter.AutoFlush <- true

    override this.Write stream =
        let baseWrite = base.Write stream // can't call base in a closure
        match stream with
        | Stream.Error -> errWriter.WriteLine
        | _ -> baseWrite

    override this.Dispose () =
        errWriter.Flush ()
        errWriter.Close ()
        base.Dispose ()

type FileLogger (path: string, ?verbosity: Stream) as this =
    inherit LoggerBase (
        File.Open (path, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.Read),
        defaultArg verbosity defaultVerbosity)

    do this.FormatMessage <- (fun stream msg ->
        let timestamp = DateTime.Now.ToString ("o")
        $"{timestamp.PadRight(33, ' ')} : {stream.ToString().PadRight(7, ' ')} : {msg}")

type CompoundLogger (loggers: ILogger list, ?verbosity: Stream) =
    inherit LoggerBase (IO.Stream.Null)

    let mutable verbosity = defaultArg verbosity defaultVerbosity
    let setChildVerbosity v = loggers |> List.iter (fun logger -> logger.Verbosity <- v)
    do setChildVerbosity verbosity

    member val Loggers = loggers

    member this.Verbosity
        with get () = verbosity
        and set (v) = setChildVerbosity v; verbosity <- v

    override this.Log (stream: Stream) =
        fun msg -> loggers |> List.iter (fun logger -> logger.Log stream msg)

    override this.Log (msg: obj) =
        loggers |> List.iter (fun logger -> logger.Log msg)

    override this.LogError (e: Exception) =
        loggers |> List.iter (fun logger -> logger.LogError e)

    override this.Dispose () =
        loggers |> List.iter (fun logger -> logger.Dispose ())

open Config

let makeLogger (config: Config) : ILogger =
    match config.logPath, config.logConsole with
    | None, false -> new NullLogger (config.verbosity)
    | None, true -> new ConsoleLogger (config.verbosity)
    | Some path, false -> new FileLogger (path, config.verbosity)
    | Some path, true -> new CompoundLogger ([
        new ConsoleLogger (config.verbosity)
        new FileLogger (path, config.verbosity)
    ], config.verbosity)
