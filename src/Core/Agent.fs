module Agent

open System.Collections.Generic
open System.Text.RegularExpressions
open MimeKit
open MailKit
open MailKit.Search
open MailKit.Net.Imap
open Grammar
open ImapRules.Sieve

type SslOptions = MailKit.Security.SecureSocketOptions

type Config = {
    server: string
    port: int
    SslOptions: SslOptions
    username: string
    password: string
}

module Helpers =
    let envelopeProperties: Map<string, System.Reflection.PropertyInfo> =
        Map(
            typeof<Envelope>.GetProperties()
            |> Array.map (fun prop -> prop.Name.ToLower(), prop)
        )

    type HeaderValue =
        | AddressList of MailboxAddress list
        | StringList of string list
        | String of string
        | Date of System.DateTimeOffset
        | NoData

    let getHeader (h: string) (e: Envelope) =
        let success, prop = envelopeProperties.TryGetValue(h.ToLower())

        if not success then
            failwith $"not a valid envelope header: '{h}'"

        let value = prop.GetValue(e)

        match prop.PropertyType with
        | T when T = typeof<InternetAddressList> ->
            (value :?> InternetAddressList).Mailboxes |> List.ofSeq |> AddressList
        | T when T = typeof<System.Nullable<System.DateTimeOffset>> ->
            let v = value :?> System.Nullable<System.DateTimeOffset>
            if v.HasValue then Date v.Value else NoData
        | T when T = typeof<string> -> value :?> string |> HeaderValue.String
        | _ -> failwith $"Failed to extract header {h} with type {prop.PropertyType.FullName}"

    let isStringMatch (mt: MatchType) (pattern: string) (sl: string list) =
        let pattern' = pattern.ToLower()

        match mt with
        | Is -> List.contains pattern' sl

        | Contains ->
            let filtered = List.filter (fun (s: string) -> s.Contains pattern') sl
            filtered <> []

        | Matches ->
            let pattern'' = Regex.Escape(pattern')
            let pattern''' = pattern''.Replace("\\?", ".").Replace("\\*", ".*")
            let filtered = List.filter (fun (s: string) -> Regex.IsMatch(s, pattern''')) sl
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
                | Date d -> [ d.ToString("o") ] // TODO: should fail?
                | NoData -> [])
            |> List.concat
            |> List.distinct
            |> List.map (fun s -> s.ToLower()) // TODO: implement case-sensitivity

        kl |> List.exists (fun k -> isStringMatch mt k headerData)

    let rec isMatch (test: Grammar.Test) (msg: IMessageSummary) =
        match test with
        | Address _
        | Envelope _
        | Header _ ->
            match test, None with
            | Address(apOpt, mtOpt, compOpt, hl, kl), _
            | Envelope(apOpt, mtOpt, compOpt, hl, kl), _
            | Header(mtOpt, compOpt, hl, kl), apOpt ->
                match compOpt with
                | None -> isHeaderMatch apOpt mtOpt hl kl msg
                | Some c -> failwith $"Not implementated yet: {c}" // TODO
            | _ -> false

        | Size _ when not msg.Size.HasValue -> false // if we forget to request Size
        | Size(sizeQual, size) ->
            let msgSize = uint msg.Size |> decimal

            match sizeQual with
            | Over -> msgSize > size
            | Under -> msgSize < size

        | AllOf [] -> true
        | AllOf(test :: tail) -> (isMatch test msg) && (isMatch (AllOf tail) msg)
        | AnyOf [] -> false
        | AnyOf(test :: tail) -> (isMatch test msg) || (isMatch (AnyOf tail) msg)

        | Not test -> not (isMatch test msg)

        | Exists hl ->
            let msgHeaders = [
                for h in msg.Headers do
                    h.Field.ToLower()
            ]

            hl |> List.exists (fun h -> List.contains (h.ToLower()) msgHeaders)

        | True -> true
        | False -> false

open Helpers

type Agent(config: Config, ?client: IImapClient) as this =
    let fetchRequest =
        MessageSummaryItems.Envelope
        ||| MessageSummaryItems.Headers
        ||| MessageSummaryItems.Size
        |> FetchRequest

    let mutable cache: IMessageSummary list = []

    let client = defaultArg client (new ImapClient())
    do this.Open()

    member this.Open() =
        client.Connect(config.server, config.port, config.SslOptions)
        let cred = System.Net.NetworkCredential(config.username, config.password)
        client.Authenticate(System.Text.Encoding.UTF8, cred)
        client.Inbox.Open(FolderAccess.ReadOnly) |> ignore

    member this.Fetch count =
        client.Inbox.Fetch(0, (count - 1), fetchRequest) |> List.ofSeq

    member this.FetchOne() = (this.Fetch 1)[0]

    member this.Query(sieveTest: string) =
        let sieveCommands = parse sieveTest

        let test =
            match sieveCommands with
            | Test t :: [] -> t
            | _ -> failwith "Sieve script was not a bare conditional test"

        let msgs = this.Fetch 10 // TODO - message cache, modseq etc
        msgs |> List.filter (isMatch test)

    member this.populateCache(count: uint16) =
        let count = int count

        let rec fib a b = seq {
            for _ in [ () ] do
                let c = a + b
                yield c
                yield! fib b c
        }

        let fibSeq = seq {
            for _ in [ () ] do
                yield 0
                yield! fib 2 3
        }

        let rec fetchBatch (sizeSeq: int seq) (count: int) =
            let min = Seq.head sizeSeq
            let sizeSeq' = Seq.skip 1 sizeSeq
            let max = Seq.head sizeSeq'

            let max = if count < (max - min) then (min + count) else max
            printf $"Fetching {count} from {min} to {max}..."
            let batch = client.Inbox.Fetch(min, max, fetchRequest)
            // TODO: when you request more than the inbox holds, throws MailKit.Net.Imap.ImapCommandException: The IMAP server replied to the 'FETCH' command with a 'BAD' response: Error in IMAP command FETCH: Invalid messageset (0.001 + 0.000 secs).
            printfn $" Found {batch.Count} messages."

            let count' = count - batch.Count
            let batch' = batch |> List.ofSeq

            match batch.Count, count' with
            | 0, _ -> batch'
            | _, c when c > 0 -> batch' @ fetchBatch sizeSeq' count'
            | _ -> batch'

        cache <- fetchBatch fibSeq count
        cache

    member this.FetchSince(date: System.DateTimeOffset) =
        let query = SearchQuery.SentSince(date.Date)
        let uids = client.Inbox.Search(query)
        printfn $"Found {uids.Count} messages"

        let rec fetchBatch (uids: IList<UniqueId>) =
            if uids.Count < 30 then
                client.Inbox.Fetch(uids, fetchRequest) |> List.ofSeq
            else
                let batchUids = uids |> Seq.truncate 20 |> List<UniqueId>
                let uids' = uids |> Seq.skip 20 |> List<UniqueId>
                (client.Inbox.Fetch(batchUids, fetchRequest) |> List.ofSeq) @ fetchBatch uids'

        fetchBatch uids

    interface System.IDisposable with
        member this.Dispose() =
            client.Disconnect(true)
            client.Dispose()
