module Agent

open MimeKit
open MailKit
open MailKit.Search
open MailKit.Net.Imap

type SslOptions = MailKit.Security.SecureSocketOptions

type Config = {
    server: string
    port: int
    SslOptions: SslOptions
    username: string
    password: string
}

type Agent(config: Config) as this =
    let client = new ImapClient()
    do this.Open()

    member this.Open() =
        client.Connect(config.server, config.port, config.SslOptions)
        client.Authenticate(config.username, config.password)
        client.Inbox.Open(FolderAccess.ReadOnly) |> ignore

    member this.FetchOne(items: MessageSummaryItems) = client.Inbox.Fetch(0, 1, items).[0]

    member this.Query (items: MessageSummaryItems) (sieveTest: string) =
        let query = new SearchQuery() // TODO
        let ids = client.Inbox.Search query
        client.Inbox.Fetch(ids, items)

    interface System.IDisposable with
        member this.Dispose() =
            client.Disconnect(true)
            client.Dispose()
