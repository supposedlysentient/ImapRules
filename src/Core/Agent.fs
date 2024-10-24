module Agent

open System.Collections.Generic
open MailKit
open MailKit.Search
open MailKit.Net.Imap
open Grammar
open Config

type Agent(config: Config, ?client: IImapClient) as this =
    let fetchRequest =
        MessageSummaryItems.Envelope
        ||| MessageSummaryItems.Headers
        ||| MessageSummaryItems.Size
        |> FetchRequest

    let client = defaultArg client (new ImapClient())
    do this.Open()

    member this.Open() =
        client.Connect(config.server, config.port, config.sslOptions)
        let cred = System.Net.NetworkCredential(config.username, config.password)
        client.Authenticate(System.Text.Encoding.UTF8, cred)
        client.Inbox.Open(FolderAccess.ReadOnly) |> ignore

    member this.Fetch count =
        client.Inbox.Fetch(0, (count - 1), fetchRequest) |> List.ofSeq

    member this.Fetch(uids: UniqueId list) =
        let uids' = List<UniqueId> uids
        client.Inbox.Fetch(uids', fetchRequest) |> List.ofSeq

    member this.Fetch(uid: UniqueId) = this.Fetch [ uid ]

    member this.FetchOne() = (this.Fetch 1)[0]

    member this.GetUidsSince(date: System.DateTimeOffset) =
        let query = SearchQuery.SentSince(date.Date)
        let uids = client.Inbox.Search(query)
        printfn $"Found {uids.Count} messages"
        uids |> List.ofSeq

    member this.FetchSince(date: System.DateTimeOffset) =
        let uids = this.GetUidsSince date
        printfn $"Found {uids.Length} messages"

        let rec fetchBatch (uids: UniqueId list) =
            if uids.Length < 30 then
                this.Fetch uids
            else
                let batchUids = uids |> List.truncate 20
                let uids' = uids |> List.skip 20
                this.Fetch batchUids @ fetchBatch uids'

        fetchBatch uids

    member this.Process (action: Action) (msg: IMessageSummary) =
        match action with
        | Reject msg -> printfn $"rejecting with {msg}"
        | Redirect address -> printfn $"redirecting to {address}"
        | Keep -> ()
        | Discard -> printfn $"discarding"
        | FileInto folder -> printfn $"filing into {folder}"

    interface System.IDisposable with
        member this.Dispose() =
            client.Disconnect(true)
            client.Dispose()
