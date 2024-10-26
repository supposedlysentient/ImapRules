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
    username: string
    password: string
    rulePath: string list
    checkpointPath: string
    logPath: string option
    logConsole: bool
    verbosity: Stream
}

module private Config =
    open Newtonsoft.Json.Linq

    module Helpers = Decode.Helpers

    type LogPath =
        | Off
        | Default
        | Path of string

    type SavedConfig = {
        server: string
        port: int option
        ssl: SslOptions option
        username: string
        password: string
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
                Enum.Parse(typeof<'T>, raw, true) :?> 'T |> Ok
            with _ ->
                let allValues: 'T array = 'T.GetValues()
                let msgPart =
                    allValues
                    |> Array.map (fun v -> v.ToString ())
                    |> String.concat (", ")

                Error (path, BadPrimitiveExtra(nameof<'T>, token, $"Expecting one of {allValues}"))

    let stringOrStringListDecoder: Decoder<string list> =
        fun path token ->
            if Helpers.isArray token then
                Helpers.asArray token
                |> Array.map (fun token -> Decode.string path token)
                |> Array.fold
                    (fun state res ->
                        match state, res with
                        | Ok prevResults, Ok res -> Ok(prevResults @ [ res ])
                        | Ok _, Error e
                        | Error e, _ -> Error e)
                    (Ok([]))
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
            | token -> Error (path, BadPrimitive("a non-empty string or (null|false|empty string)", token))

    let decoder: Decoder<SavedConfig> =
        Decode.object (fun get -> {
            server = get.Required.Field "server" Decode.string
            port = get.Optional.Field "port" Decode.int
            ssl = get.Optional.Field "ssl" enumDecoder<SslOptions>
            username = get.Required.Field "username" Decode.string
            password = get.Required.Field "password" Decode.string
            rule_path = get.Optional.Field "rule_path" stringOrStringListDecoder
            checkpoint_path = get.Optional.Field "checkpoint_path" Decode.string
            log_path = get.Required.Raw (logPathDecoder "log_path")
            log_console = get.Optional.Field "log_console" Decode.bool
            verbosity = get.Optional.Field "verbosity" enumDecoder<Stream>
        })

let read (path: string) : Config =
    let json = File.ReadAllText(path)

    let config =
        match Decode.fromString Config.decoder json with
        | Error e -> failwith e
        | Ok c -> c

    let port, sslOptions =
        match config.port, config.ssl with
        | None, None -> 993, SslOptions.StartTls
        | None, Some(SslOptions.None as o)
        | None, Some(SslOptions.SslOnConnect as o) -> 143, o
        | None, Some o -> 993, o
        | Some p, None -> p, SslOptions.Auto
        | Some p, Some o -> p, o

    let defaultRulePath = "*.sieve"
    let defaultCheckpoint = "checkpoint"
    let defaultLogPath = "ImapRules.log"
    let basePath = Path.GetDirectoryName path
    let checkpoint = defaultArg config.checkpoint_path (Path.Combine(basePath, defaultCheckpoint))
    let logPath =
        match config.log_path with
        | Config.LogPath.Path p -> Path.Combine(basePath, p) |> Some
        | Config.LogPath.Default -> Path.Combine(basePath, defaultLogPath) |> Some
        | Config.LogPath.Off -> None

    let rulePaths =
        match config.rule_path with
        | None -> [ defaultRulePath ]
        | Some p -> p
        |> List.map (fun path -> Directory.GetFiles(basePath, path))
        |> Array.concat
        |> List.ofArray

    {
        server = config.server
        port = port
        sslOptions = sslOptions
        username = config.username
        password = config.password
        rulePath = rulePaths
        checkpointPath = checkpoint
        logPath = logPath
        logConsole = defaultArg config.log_console true
        verbosity = defaultArg config.verbosity Stream.Output
    }
