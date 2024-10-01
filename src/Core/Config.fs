module Config

open System.IO
open FSharp.Json

type Config = Agent.Config

let read (path: string) =
    let json = File.ReadAllText(path)
    Json.deserialize<Config> json
