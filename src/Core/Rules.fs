module Rules

open System.IO
open FSharp.Json
open MailKit
open Grammar
open ImapRules.Sieve

let read (path: string) =
    let content = File.ReadAllText(path)
    parse content
