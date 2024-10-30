module Helpers

open Fake.Core

let initializeContext () =
    let execContext = Context.FakeExecutionContext.Create false "build.fsx" []
    Context.setExecutionContext (Context.RuntimeContext.Fake execContext)

module Proc =
    module Parallel =
        open System

        let locker = obj ()

        let colors = [|
            ConsoleColor.Blue
            ConsoleColor.Yellow
            ConsoleColor.Magenta
            ConsoleColor.Cyan
            ConsoleColor.DarkBlue
            ConsoleColor.DarkYellow
            ConsoleColor.DarkMagenta
            ConsoleColor.DarkCyan
        |]

        let print color (colored: string) (line: string) =
            lock locker (fun () ->
                let currentColor = Console.ForegroundColor
                Console.ForegroundColor <- color
                Console.Write colored
                Console.ForegroundColor <- currentColor
                Console.WriteLine line)

        let onStdout index name (line: string) =
            let color = colors[index % colors.Length]

            if isNull line then
                print color $"{name}: --- END ---" ""
            else if String.isNotNullOrEmpty line then
                print color $"{name}: " line

        let onStderr name (line: string) =
            let color = ConsoleColor.Red

            if isNull line |> not then
                print color $"{name}: " line

        let redirect (index, (name, createProcess)) =
            createProcess
            |> CreateProcess.redirectOutputIfNotRedirected
            |> CreateProcess.withOutputEvents (onStdout index name) (onStderr name)

        let printStarting indexed =
            for (index, (name, c: CreateProcess<_>)) in indexed do
                let color = colors[index % colors.Length]
                let wd = c.WorkingDirectory |> Option.defaultValue ""
                let exe = c.Command.Executable
                let args = c.Command.Arguments.ToStartInfo
                print color $"{name}: {wd}> {exe} {args}" ""

        let run cs =
            cs
            |> Seq.toArray
            |> Array.indexed
            |> fun x ->
                printStarting x
                x
            |> Array.map redirect
            |> Array.Parallel.map Proc.run

let createProcess exe args dir =
    // Use `fromRawCommand` rather than `fromRawCommandLine`, as its behaviour is less likely to be misunderstood.
    // See https://github.com/SAFE-Stack/SAFE-template/issues/551.
    CreateProcess.fromRawCommand exe args
    |> CreateProcess.withWorkingDirectory dir
    |> CreateProcess.ensureExitCode

let dotnet args dir = createProcess "dotnet" args dir

let docker args dir = createProcess "docker" args dir

let run proc arg dir = proc arg dir |> Proc.run |> ignore

let runParallel processes =
    processes |> Proc.Parallel.run |> ignore

let runOrDefault args =
    try
        match args with
        | [||] -> Target.runOrDefault "Run"
        | _ ->
            let target = Array.head args
            args
            |> Array.skip 1
            |> List.ofArray
            |> Target.run 1 target
        0
    with e ->
        printfn "%A" e
        1

let ignoreCase = System.StringComparison.OrdinalIgnoreCase

module Docker =
    type OciLabels =
        {
            authors: string
            description: string
            documentation: string
            licenses: string
            source: string
            title: string
            url: string
            vendor: string
            version: string
        }

        static member defaults =
            {
                authors = "imaprules@sckr.link"
                description = "IMAP agent that connects to your mailbox and runs mail rules."
                documentation = "https://github.com/fsackur/ImapRules/README.md"
                licenses = "https://github.com/fsackur/ImapRules/LICENSE"
                source = "https://github.com/fsackur/ImapRules/"
                title = "ImapRules"
                url = "https://github.com/fsackur/ImapRules/"
                vendor = "Freddie Sackur"
                version = ""
            }

        static member ofVersion (version: string) =
            { OciLabels.defaults with version = version }

    open FSharp.Reflection

    let toDockerArgs (labels: OciLabels) =
        FSharpType.GetRecordFields (labels.GetType())
        |> Array.map (fun pi -> [|
            let value = pi.GetValue labels
            "--label"
            $"org.opencontainers.image.{pi.Name}=\"{value}\"" |])
        |> Array.concat
        |> List.ofArray

    open System

    let validateSemVer (version: string) =
        let success, version' = Version.TryParse (version.Replace("v", "", ignoreCase))
        if not success || version'.Revision <> -1 || version'.Build = -1 then
            failwith $"Failed to parse version: '{version}'"
