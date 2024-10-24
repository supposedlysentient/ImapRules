module Config

open System.IO
open Thoth.Json.Net

type SslOptions = MailKit.Security.SecureSocketOptions

type Config = {
    server: string
    port: int
    sslOptions: SslOptions
    username: string
    password: string
    rulePath: string list
}

module private Config =
    module Helpers = Decode.Helpers

    type SavedConfig = {
        server: string
        port: int option
        ssl: SslOptions option
        username: string
        password: string
        rule_path: string list option
    }

    let sslDecoder: Decoder<SslOptions> =
        fun path token ->
            try
                let raw = Helpers.asString token
                SslOptions.Parse(raw, true) |> Ok
            with _ ->
                let allValues =
                    SslOptions.GetValues()
                    |> Array.map (fun (v: SslOptions) -> v.ToString())
                    |> String.concat (", ")

                (path, BadPrimitiveExtra("SecureSocketOptions", token, $"Expecting one of {allValues}"))
                |> Error

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

    let decoder: Decoder<SavedConfig> =
        Decode.object (fun get -> {
            server = get.Required.Field "server" Decode.string
            port = get.Optional.Field "port" Decode.int
            ssl = get.Optional.Field "ssl" sslDecoder
            username = get.Required.Field "username" Decode.string
            password = get.Required.Field "password" Decode.string
            rule_path = get.Optional.Field "rule_path" stringOrStringListDecoder
        })

let defaultRulePath = "*.sieve"

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

    let basePath = Path.GetDirectoryName path

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
    }
