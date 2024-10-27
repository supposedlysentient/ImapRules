module Rules

open System.IO
open System.Text.RegularExpressions
open MimeKit
open MailKit
open Grammar
open Agent
open ImapRules.Sieve

let read (path: string) =
    let basePath = Path.GetDirectoryName path
    let file = Path.GetFileName path
    System.IO.Directory.GetFiles (basePath, file)
    |> Array.map File.ReadAllText
    |> Array.map parse
    |> List.concat

module Helpers =
    let envelopeProperties: Map<string, System.Reflection.PropertyInfo> =
        Map (
            typeof<Envelope>.GetProperties ()
            |> Array.map (fun prop -> prop.Name.ToLower (), prop)
        )

    type HeaderValue =
        | AddressList of MailboxAddress list
        | StringList of string list
        | String of string
        | Date of System.DateTimeOffset
        | NoData

    let getHeader (h: string) (e: Envelope) =
        let success, prop = envelopeProperties.TryGetValue (h.ToLower ())

        if not success then
            failwith $"not a valid envelope header: '{h}'"

        let value = prop.GetValue e

        match prop.PropertyType with
        | T when T = typeof<InternetAddressList> ->
            (value :?> InternetAddressList).Mailboxes |> List.ofSeq |> AddressList
        | T when T = typeof<System.Nullable<System.DateTimeOffset>> ->
            let v = value :?> System.Nullable<System.DateTimeOffset>
            if v.HasValue then Date v.Value else NoData
        | T when T = typeof<string> -> value :?> string |> HeaderValue.String
        | _ -> failwith $"Failed to extract header {h} with type {prop.PropertyType.FullName}"

    let isStringMatch (mt: MatchType) (pattern: string) (sl: string list) =
        let pattern' = pattern.ToLower ()

        match mt with
        | Is -> List.contains pattern' sl

        | Contains ->
            let filtered = List.filter (fun (s: string) -> s.Contains pattern') sl
            filtered <> []

        | Matches ->
            let pattern'' = Regex.Escape pattern'
            let pattern''' = pattern''.Replace("\\?", ".").Replace("\\*", ".*")
            let filtered = List.filter (fun (s: string) -> Regex.IsMatch (s, pattern''')) sl
            filtered <> []

    let isHeaderMatch
        (apOpt: AddressPart option)
        (mtOpt: MatchType option)
        (hl: Header list)
        (kl: string list)
        (msg: IMessageSummary)
        =
        let mt =
            match mtOpt with
            | None -> Is
            | Some mt -> mt

        let headerData =
            [
                for h in hl do
                    getHeader h msg.Envelope
            ]
            |> List.map (function
                | AddressList al ->
                    let transform: MailboxAddress -> string =
                        match apOpt with
                        | None
                        | Some All -> fun mb -> mb.Address
                        | Some LocalPart -> fun mb -> mb.LocalPart
                        | Some Domain -> fun mb -> mb.Domain

                    al |> List.map transform
                | StringList sl -> sl
                | String s -> [ s ]
                | Date d -> [ d.ToString ("o") ] // TODO: should fail?
                | NoData -> [])
            |> List.concat
            |> List.distinct
            |> List.map (fun s -> s.ToLower ()) // TODO: implement case-sensitivity

        kl |> List.exists (fun k -> isStringMatch mt k headerData)

open Helpers

let rec isMatch (test: Grammar.Test) (msg: IMessageSummary) =
    match test with
    | Address _
    | Envelope _
    | Header _ ->
        match test, None with
        | Address (apOpt, mtOpt, compOpt, hl, kl), _
        | Envelope (apOpt, mtOpt, compOpt, hl, kl), _
        | Header (mtOpt, compOpt, hl, kl), apOpt ->
            match compOpt with
            | None -> isHeaderMatch apOpt mtOpt hl kl msg
            | Some c -> failwith $"Not implementated yet: {c}" // TODO
        | _ -> false

    | Size _ when not msg.Size.HasValue -> false // if we forget to request Size
    | Size (sizeQual, size) ->
        let msgSize = uint msg.Size |> decimal

        match sizeQual with
        | Over -> msgSize > size
        | Under -> msgSize < size

    | AllOf [] -> true
    | AllOf (test :: tail) -> (isMatch test msg) && (isMatch (AllOf tail) msg)
    | AnyOf [] -> false
    | AnyOf (test :: tail) -> (isMatch test msg) || (isMatch (AnyOf tail) msg)

    | Not test -> not (isMatch test msg)

    | Exists hl ->
        let msgHeaders = [
            for h in msg.Headers do
                h.Field.ToLower ()
        ]

        hl |> List.exists (fun h -> List.contains (h.ToLower ()) msgHeaders)

    | True -> true
    | False -> false

[<TailCall>]
let rec private process' (agent: Agent) (cmd: Command) (msg: IMessageSummary) =
    match cmd with
    | Require r -> printfn $"requiring {r}"
    | Action a -> agent.Process a msg
    | Control (If (ifBlock, elseIfBlocks, elseBlock)) ->
        let conditionals = ifBlock :: elseIfBlocks @ [ Conditional (True, elseBlock) ]

        let matchingConds =
            conditionals
            |> List.skipWhile (fun (Conditional (test, _)) -> isMatch test msg |> not)

        match matchingConds with
        | [] -> ()
        | firstMatch :: _ ->
            let (Conditional (_, actions)) = firstMatch
            actions |> List.iter (fun a -> agent.Process a msg)
    | Test t -> printfn $"test {t}"

let processMessage (agent: Agent) (rules: Command list) (msg: IMessageSummary) =
    try
        rules |> List.iter (fun rule -> process' agent rule msg)
    with StopProcessing -> ()
