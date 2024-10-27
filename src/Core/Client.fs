module Client

open System
open System.Collections.Generic
open MailKit
open MailKit.Net.Imap
open MailKit.Search
open Grammar
open Config
open Logging

let fetchRequest =
    MessageSummaryItems.Envelope
    ||| MessageSummaryItems.Headers
    ||| MessageSummaryItems.Size
    |> FetchRequest

type IClient =
    abstract Fetch: UniqueId list -> IMessageSummary list
    abstract Search: SearchQuery -> UniqueId list
    abstract Delete: UniqueId -> unit
    abstract MoveTo: UniqueId * string -> unit
    abstract UidNext: UniqueId
    abstract UidValidity: uint
    inherit IDisposable

type Client (config: Config, logger: ILogger, ?imapClient: IImapClient) =
    let _client = defaultArg imapClient (new ImapClient (logger))
    let mutable _inbox: IMailFolder option = None

    member private this.Client with get () =
        if not _client.IsConnected then
            _client.Connect (config.server, config.port, config.sslOptions)
        if not _client.IsAuthenticated then
            let cred = System.Net.NetworkCredential (config.username, config.password)
            _client.Authenticate (System.Text.Encoding.UTF8, cred)
        _client

    member private this.Inbox with get () =
        match _inbox with
        | Some inbox -> inbox
        | None ->
            let inbox = this.Client.Inbox
            if not inbox.IsOpen then
                inbox.Open (FolderAccess.ReadWrite) |> ignore
            _inbox <- Some inbox
            inbox

    interface IClient with
        member this.Fetch uids =
            let msgs = this.Inbox.Fetch (List<UniqueId> uids, fetchRequest)
            if msgs = null then [] else msgs |> List.ofSeq

        member this.Search query =
            this.Inbox.Search (query) |> List.ofSeq

        member this.Delete uid =
            let request = StoreFlagsRequest (StoreAction.Add, MessageFlags.Deleted)
            if this.Inbox.Store (uid, request) then
                this.Inbox.Expunge ()
            else
                failwith $"Failed to mark as deleted: {uid}"

        member this.MoveTo (uid, folder: string) =
            let folder' = this.Client.GetFolder (folder)
            this.Inbox.MoveTo (uid, folder') |> ignore

        member this.UidNext with get () =
            if this.Inbox.UidNext.HasValue then
                this.Inbox.UidNext.Value
            else
                failwith "UidNext is null; is the client connected?"

        member this.UidValidity with get () =
            this.Inbox.UidValidity

    interface IDisposable with
        member this.Dispose () =
            match _inbox with
            | None -> ()
            | Some inbox -> inbox.Close ()
            _client.Dispose ()
