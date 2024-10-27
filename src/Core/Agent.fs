module Agent

open System.Collections.Generic
open MailKit
open MailKit.Search
open Grammar
open Config
open Logging
open Checkpoint

exception StopProcessing


let fetchRequest =
    MessageSummaryItems.Envelope
    ||| MessageSummaryItems.Headers
    ||| MessageSummaryItems.Size
    |> FetchRequest

type Agent (
    config: Config,
    ?logger: ILogger,
    ?client: Net.Imap.IImapClient,
    ?checkpoint: ICheckpoint) as this =

    let logger = defaultArg logger (makeLogger config)
    let client = defaultArg client (new Net.Imap.ImapClient (logger))
    let checkpoint = defaultArg checkpoint (new Checkpoint (config.checkpointPath))
    do this.Open ()

    member this.Open () =
        client.Connect (config.server, config.port, config.sslOptions)
        let cred = System.Net.NetworkCredential (config.username, config.password)
        client.Authenticate (System.Text.Encoding.UTF8, cred)
        client.Inbox.Open (FolderAccess.ReadWrite) |> ignore

    member this.Fetch count =
        client.Inbox.Fetch (0, (count - 1), fetchRequest) |> List.ofSeq

    member this.Fetch (uids: UniqueId list) =
        let uids' = List<UniqueId> uids
        client.Inbox.Fetch (uids', fetchRequest) |> List.ofSeq

    member this.Fetch (uid: UniqueId) = this.Fetch [ uid ]

    member this.FetchOne () = (this.Fetch 1)[0]

    member this.GetUidsSince (date: System.DateTimeOffset) =
        let query = SearchQuery.SentSince (date.Date)
        let uids = client.Inbox.Search (query)
        uids |> List.ofSeq

    member this.GetUidsSinceCheckpoint () =
        let id = checkpoint.Read ()
        logger.Log Stream.Info $"Read checkpoint uid: {id}"
        let nextId =
            if client.Inbox.UidNext.HasValue then
                client.Inbox.UidNext.Value.Id
            else
                failwith "Next UID is null; is the client connected?"

        if nextId > id then
            [ id .. nextId ]
            |> List.skip 1 // checkpoint represents last message processed
            |> List.map (fun id -> UniqueId (client.Inbox.UidValidity, id))
        else
            []

    member this.FetchSince (date: System.DateTimeOffset) =
        let uids = this.GetUidsSince date
        logger.Log Stream.Info $"Found {uids.Length} message(s)"
        this.Fetch uids

    member this.Process (action: Action) (msg: IMessageSummary) =
        let uid = msg.UniqueId
        let repr = $"<Uid: {uid}; Sent: {msg.Envelope.Date}>"
        match action with
        | Reject rejMsg -> logger.Log Stream.Info $"Rejecting with '{rejMsg}': {repr}"
        | Redirect address -> logger.Log Stream.Info $"Redirecting to '{address}': {repr}"
        | Keep -> ()
        | Discard ->
            logger.Log Stream.Info $"Discarding: {repr}"
            let request = StoreFlagsRequest (StoreAction.Add, MessageFlags.Deleted)
            if client.Inbox.Store (uid, request) then
                client.Inbox.Expunge ()
            else
                failwith $"Failed to mark as deleted: {repr}"
        | FileInto f ->
            logger.Log Stream.Info $"Filing into {f}: {repr}"
            let folder = this.GetFolder f
            client.Inbox.MoveTo (uid, folder) |> ignore
        | Stop ->
            logger.Log Stream.Info $"Stopping processing rules: {repr}"
            raise StopProcessing

    member this.Checkpoint (uid: UniqueId) =
        checkpoint.Write uid.Id

    member this.GetFolder (path: string) =
        client.GetFolder (path)

    interface System.IDisposable with
        member this.Dispose () =
            if client.IsConnected then client.Disconnect (true)
            client.Dispose ()
