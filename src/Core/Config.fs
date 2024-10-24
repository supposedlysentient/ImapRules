module Config

open System.IO
open FSharp.Json

type Config = Agent.Config

let read (path: string) =
    let json = File.ReadAllText(path)
    let config = Json.deserialize<Config> json

    let basePath = Path.GetDirectoryName path

    let rulePaths =
        config.rulePath
        |> List.map (fun path -> Directory.GetFiles(basePath, path))
        |> Array.concat
        |> List.ofArray

    { config with rulePath = rulePaths }
