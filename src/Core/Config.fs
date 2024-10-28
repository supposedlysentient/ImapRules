module Config

open System
open System.IO
open Thoth.Json.Net

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
    open Newtonsoft.Json.Linq

    module Helpers = Decode.Helpers

    type LogPath =
        | Off
        | Default
        | Path of string

    type JsonConfig = {
        server: string
        port: int option
        ssl: SslOptions option
        username: string option
        password: string option
        credential_path: string option
        rule_path: string list option
        checkpoint_path: string option
        log_path: LogPath
        log_console: bool option
        verbosity: Stream option
    }

    let enumDecoder<'T when 'T : struct and 'T :> Enum and 'T : (new: unit -> 'T)> : Decoder<'T> =
        fun path token ->
            try
                let raw =
                    if Helpers.isNumber token then
                        Helpers.asInt token |> string
                    else
                        Helpers.asString token
                Enum.Parse (typeof<'T>, raw, true) :?> 'T |> Ok
            with _ ->
                let allValues: 'T array = 'T.GetValues ()
                let msgPart =
                    allValues
                    |> Array.map (fun v -> v.ToString ())
                    |> String.concat (", ")

                Error (path, BadPrimitiveExtra (nameof<'T>, token, $"Expecting one of {allValues}"))

    let stringOrStringListDecoder: Decoder<string list> =
        fun path token ->
            if Helpers.isArray token then
                Helpers.asArray token
                |> Array.map (fun token -> Decode.string path token)
                |> Array.fold
                    (fun state res ->
                        match state, res with
                        | Ok prevResults, Ok res -> Ok (prevResults @ [ res ])
                        | Ok _, Error e
                        | Error e, _ -> Error e)
                    (Ok [])
            else
                [ Helpers.asString token ] |> Ok

    let logPathDecoder fieldName =
        fun path token ->
            match Helpers.getField fieldName token with
            | null -> Ok Default
            | token when token.Type = JTokenType.Null -> Ok Off
            | token when token.Type = JTokenType.String ->
                let path = Helpers.asString token
                if path = "" then Off else Path path
                |> Ok
            | token when token.Type = JTokenType.Boolean && token.Value<bool>() = false -> Ok Off
            | token -> Error (path, BadPrimitive ("a non-empty string or (null|false|empty string)", token))

    let decoder: Decoder<JsonConfig> =
        Decode.object (fun get -> {
            server = get.Required.Field "server" Decode.string
            port = get.Optional.Field "port" Decode.int
            ssl = get.Optional.Field "ssl" enumDecoder<SslOptions>
            username = get.Optional.Field "username" Decode.string
            password = get.Optional.Field "password" Decode.string
            credential_path = get.Optional.Field "credential_path" Decode.string
            rule_path = get.Optional.Field "rule_path" stringOrStringListDecoder
            checkpoint_path = get.Optional.Field "checkpoint_path" Decode.string
            log_path = get.Required.Raw (logPathDecoder "log_path")
            log_console = get.Optional.Field "log_console" Decode.bool
            verbosity = get.Optional.Field "verbosity" enumDecoder<Stream>
        })

let read (path: string) : Config =
    let json = File.ReadAllText (path)

    let config =
        match Decode.fromString Config.decoder json with
        | Error e -> failwith e
        | Ok c -> c

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
        | Config.LogPath.Path p -> Path.Combine (basePath, p) |> Some
        | Config.LogPath.Default -> Path.Combine (basePath, defaults.logPath) |> Some
        | Config.LogPath.Off -> None

    let rulePaths =
        match config.rule_path with
        | None -> defaults.rulePath
        | Some path -> path
        |> List.map (fun path -> Directory.GetFiles (basePath, path))
        |> Array.concat
        |> List.ofArray

    let readCred path =
        let lines = File.ReadAllLines(path) |> Array.filter (
            fun s -> s.Length > 0 && not (s.TrimStart().StartsWith('#')))
        if lines.Length <> 2 then
            failwith $"Credentials at {path} should contain username and password on separate lines"
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
                failwith "No credentials supplied"
    {
        server = config.server
        port = port
        sslOptions = sslOptions
        credential = cred
        rulePath = rulePaths
        checkpointPath = checkpoint
        logPath = logPath
        logConsole = defaultArg config.log_console defaults.logConsole
        verbosity = defaultArg config.verbosity Stream.Output
    }
