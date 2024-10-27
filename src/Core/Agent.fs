module Agent

open MailKit
open MailKit.Search
open Grammar
open Config
open Logging
open Client
open Checkpoint

exception StopProcessing

let makeUids (validity: uint) (ids: uint seq) =
    [ for id in ids do UniqueId (validity, id) ]

type Agent (
    config: Config,
    ?logger: ILogger,
    ?client: IClient,
    ?checkpoint: ICheckpoint) =

    let logger = defaultArg logger (makeLogger config)
    let client = defaultArg client (new Client (config, logger))
    let checkpoint = defaultArg checkpoint (new Checkpoint (config.checkpointPath))

    member this.Fetch count =
        let next = client.UidNext.Id
        let first = next - uint count
        let uids = [ first .. next - 1u ] |> makeUids client.UidValidity
        this.Fetch uids

    member this.Fetch (uids: UniqueId list) =
        let msgs = client.Fetch uids
        logger.Log Stream.Info $"Found {msgs.Length} message(s)"
        msgs

    member this.Fetch (uid: UniqueId) = this.Fetch [ uid ]

    member this.GetUidsSince (date: System.DateTimeOffset) =
        let query = SearchQuery.SentSince (date.Date)
        let uids = client.Search (query)
        logger.Log Stream.Info $"Found {uids.Length} message uid(s)"
        uids

    member this.FetchSince (date: System.DateTimeOffset) =
        this.GetUidsSince date |> this.Fetch

    member this.GetUidsSinceCheckpoint () =
        let id = checkpoint.Read ()
        logger.Log Stream.Info $"Read checkpoint uid: {id}"
        let nextId = client.UidNext.Id
        let uids =
            if nextId > id then
                [ id .. nextId ]
                |> List.skip 1 // checkpoint represents last message processed
                |> List.map (fun id -> UniqueId (client.UidValidity, id))
            else
                []
        logger.Log Stream.Info $"Predicted {uids.Length} message uid(s)"
        uids

    member this.FetchSinceCheckpoint () =
        this.GetUidsSinceCheckpoint () |> this.Fetch

    member this.Process (action: Action) (msg: IMessageSummary) =
        let uid = msg.UniqueId
        let repr = $"<Uid: {uid}; Sent: {msg.Envelope.Date}>"
        match action with
        | Reject rejMsg -> logger.Log Stream.Info $"Rejecting with '{rejMsg}': {repr}"
        | Redirect address -> logger.Log Stream.Info $"Redirecting to '{address}': {repr}"
        | Keep -> ()
        | Discard ->
            logger.Log Stream.Info $"Discarding: {repr}"
            client.Delete uid
        | FileInto folder ->
            logger.Log Stream.Info $"Filing into {folder}: {repr}"
            client.MoveTo (uid, folder)
        | Stop ->
            logger.Log Stream.Info $"Stopping processing rules: {repr}"
            raise StopProcessing

    member this.Checkpoint (uid: UniqueId) =
        checkpoint.Write uid.Id

    interface System.IDisposable with
        member this.Dispose () =
            client.Dispose ()
