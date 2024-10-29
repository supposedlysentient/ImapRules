module Config

open System
open System.IO

type SslOptions = MailKit.Security.SecureSocketOptions

type Stream =
    | Error = 0
    | Warning = 1
    | Output = 2
    | Info = 3
    | Verbose = 4
    | Debug = 5

type Config = {
    server: string
    port: int
    sslOptions: SslOptions
    credential: System.Net.NetworkCredential
    rulePath: string list
    checkpointPath: string
    logPath: string option
    logConsole: bool
    verbosity: Stream
}

exception ConfigError of Message: string

let defaults = {|
    port = 993
    portInsecure = 143
    ssl = SslOptions.Auto
    rulePath = [ "*.sieve" ]
    credentialPath = "credential"
    checkpointPath = "checkpoint"
    logPath = "ImapRules.log"
    logConsole = true
    verbosity = Stream.Output
|}

module private Config =
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq

    type JsonConfig = {
        server: string option
        port: int option
        ssl: SslOptions option
        username: string option
        password: string option
        credential_path: string option
        rule_path: string list option
        checkpoint_path: string option
        log_path: string option
        log_console: bool option
        verbosity: Stream option
    }

    let readEnvToTokenOrNull (field: string) : JToken =
        let envVar = $"IMAPRULES_{field.ToUpper()}"
        let envVal = Environment.GetEnvironmentVariable envVar
        match envVal with
        | null
        | "" -> null
        | _ -> JToken.Parse $"\"{envVal}\"" // future parsing is quite catholic, ints will be fine

    let parseEnum<'T> (token: JToken) : 'T =
        let stringValue = token.Value<string>()
        let success, value = Enum.TryParse (typeof<'T>, stringValue, true)
        if success then
            value :?> 'T
        else
            let expected =
                Enum.GetValues (typeof<'T>) :?> 'T array
                |> Array.map (fun v -> v.ToString().ToLower())
                |> String.concat (" | ")
            $"expected ( {expected} ) but got '{stringValue}'" |> ConfigError |> raise

    let parseToken (token: JToken) : 'T =
        token.Value<'T>()

    let parseTokenAsList (token: JToken) : 'T list =
        token.Value<JArray>()
        |> Seq.map (fun token -> token.Value<'T>())
        |> List.ofSeq

    let parseList (token: JToken) : 'T list =
        if token.Type = JTokenType.Array then
            parseTokenAsList token
        else
            [ parseToken token ]

    let readField (root: JToken) (field: string) : Option<JToken> =
        match root.Item(field) with
        | null -> readEnvToTokenOrNull field
        | token -> token
        |> Option.ofObj

    let parse json =
        let serializer = JsonSerializer.Create()
        let reader = new JsonTextReader(new StringReader(json))
        let parsedJson = serializer.Deserialize<JToken>(reader)

        let read field =
            readField parsedJson field
            |> Option.map parseToken
        let readEnum field =
            readField parsedJson field
            |> Option.map parseEnum
        let readList field =
            readField parsedJson field
            |> Option.map parseList

        {
            server = read "server"
            port = read "port"
            ssl = readEnum "ssl"
            username = read "username"
            password = read "password"
            credential_path = read "credential_path"
            rule_path = readList "rule_path" // string or list
            checkpoint_path = read "checkpoint_path"
            log_path = read "log_path"
            log_console = read "log_console"
            verbosity = readEnum "verbosity"
        }


let read (path: string) =
    // It's possible that all config is passed in env vars
    let json = if Path.Exists path then File.ReadAllText (path) else "{}"

    let config = Config.parse json

    let server =
        match config.server with
        | Some s -> s
        | Some ""
        | None -> $"Missing config: 'server'. Checked {path} and ${{IMAPRULES_SERVER}}." |> ConfigError |> raise

    let port, sslOptions =
        match config.port, config.ssl with
        | None, None -> defaults.port, defaults.ssl
        | None, Some (SslOptions.None as o) -> defaults.portInsecure, o
        | None, Some o -> defaults.port, o
        | Some p, None -> p, defaults.ssl
        | Some p, Some o -> p, o

    let basePath = Path.GetDirectoryName path
    let checkpoint = defaultArg config.checkpoint_path (Path.Combine (basePath, defaults.checkpointPath))
    let logPath =
        match config.log_path with
        | Some "" -> None
        | Some path -> Path.Combine (basePath, path) |> Some
        | None -> Path.Combine (basePath, defaults.logPath) |> Some

    let rulePaths =
        match config.rule_path with
        | None -> defaults.rulePath
        | Some paths -> paths
        |> List.map (fun path -> Path.Combine (basePath, path))

    let readCred path =
        let lines = File.ReadAllLines(path) |> Array.filter (
            fun s -> s.Length > 0 && not (s.TrimStart().StartsWith('#')))
        if lines.Length <> 2 then
            $"Credentials at {path} should contain username and password on separate lines" |> ConfigError |> raise
        System.Net.NetworkCredential (lines[0], lines[1])

    let cred =
        match config.username, config.password, config.credential_path with
        | Some u, Some p, None ->
            System.Net.NetworkCredential (u, p)
        | _, _, Some path ->
            let path = Path.Combine (basePath, path)
            readCred path
        | _ ->
            let path = Path.Combine (basePath, defaults.credentialPath)
            if File.Exists path then
                readCred path
            else
                "No credentials supplied" |> ConfigError |> raise

    {
        server = server
        port = port
        sslOptions = sslOptions
        credential = cred
        rulePath = rulePaths
        checkpointPath = checkpoint
        logPath = logPath
        logConsole = defaultArg config.log_console defaults.logConsole
        verbosity = defaultArg config.verbosity Stream.Output
    }
