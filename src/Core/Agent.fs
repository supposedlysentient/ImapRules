module Agent

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
    let getEnvelopeAddress (h: string) (e: Envelope) =
        let al =
            match h.ToLower() with
            | "to" -> e.To
            | "replyto" -> e.ReplyTo
            | "from" -> e.From
            | "sender" -> e.Sender
            | "cc" -> e.Cc
            | "bcc" -> e.Bcc
            | _ -> failwith "not a valid address list header: '{h}'"

        [
            for a in al do
                match a with
                | :? MailboxAddress as a' -> a'.Address
                | a -> string a
        ]

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

    let isHeaderMatch (mtOpt: MatchType option) (hl: Header list) (kl: string list) (msg: IMessageSummary) =
        let mt =
            match mtOpt with
            | None -> Is
            | Some mt -> mt

        let addresses =
            [
                for h in hl do
                    getEnvelopeAddress h msg.Envelope
            ]
            |> List.concat
            |> List.distinct
            |> List.map (fun s -> s.ToLower())

        let filtered =
            [
                for key in kl do
                    isStringMatch mt key addresses
            ]
            |> List.filter (fun b -> b)

        filtered <> []

    let rec isMatch (test: Grammar.Test) (msg: IMessageSummary) =
        match test with
        | Address(apOpt, mtOpt, compOpt, hl, kl)
        | Envelope(apOpt, mtOpt, compOpt, hl, kl) -> isHeaderMatch mtOpt hl kl msg // TODO: implement AddressPart
        | Header(mtOpt, compOpt, hl, kl) -> isHeaderMatch mtOpt hl kl msg // TODO: implement Comparator

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

            let filtered =
                [
                    for h in hl do
                        List.contains (h.ToLower()) msgHeaders
                ]
                |> List.filter (fun b -> b)

            filtered <> []

        | True -> true
        | False -> false

open Helpers

type Agent(config: Config, ?client: IImapClient) as this =
    let metadataFields =
        MessageSummaryItems.Envelope
        ||| MessageSummaryItems.Headers
        ||| MessageSummaryItems.Size

    let client = defaultArg client (new ImapClient())
    do this.Open()

    member this.Open() =
        client.Connect(config.server, config.port, config.SslOptions)
        let cred = System.Net.NetworkCredential(config.username, config.password)
        client.Authenticate(System.Text.Encoding.UTF8, cred)
        client.Inbox.Open(FolderAccess.ReadOnly) |> ignore

    member this.Fetch count =
        client.Inbox.Fetch(0, (count - 1), metadataFields) |> List.ofSeq

    member this.FetchOne() = (this.Fetch 1)[0]

    member this.Query(sieveTest: string) =
        let sieveCommands = parse sieveTest

        let test =
            match sieveCommands with
            | Test t :: [] -> t
            | _ -> failwith "Sieve script was not a bare conditional test"

        let msgs = this.Fetch 10 // TODO - message cache, modseq etc
        msgs |> List.filter (isMatch test)

    interface System.IDisposable with
        member this.Dispose() =
            client.Disconnect(true)
            client.Dispose()
