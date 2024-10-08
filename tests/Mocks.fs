module Mocks

open System.Collections.Generic
open System.Threading
open MailKit
open MailKit.Net.Imap
open Agent

let config = {
    server = "mail.example.com"
    port = 143
    SslOptions = SslOptions.None
    username = "mockuser"
    password = "mockpass"
}

let ctor =
    typeof<ImapFolderConstructorArgs>
        .GetConstructors(
            System.Reflection.BindingFlags.NonPublic
            ||| System.Reflection.BindingFlags.Instance
        )
    |> Seq.filter (fun c -> c.GetParameters() = [||])
    |> Seq.head

let inboxArgs = ctor.Invoke([||]) :?> ImapFolderConstructorArgs

type MockFolder(args) =
    inherit ImapFolder(args)
    let mutable fetchCallCount = 0

    override this.Fetch(min: int, max: int, request: IFetchRequest, ct: CancellationToken) =
        fetchCallCount <- fetchCallCount + 1
        [| MessageSummary(0) |]

    override this.Open(access: FolderAccess, ct: CancellationToken) = access
    member this.FetchCallCount = fetchCallCount

let inbox = MockFolder inboxArgs

type MockClient() =
    inherit ImapClient()
    let mutable connectCallCount = 0
    let mutable authCallCount = 0
    override this.Inbox = inbox

    override this.Connect(host: string, port: int, options: SslOptions, ct: CancellationToken) =
        connectCallCount <- connectCallCount + 1

    override this.Authenticate(encoding: System.Text.Encoding, creds: System.Net.ICredentials, ct: CancellationToken) =
        authCallCount <- authCallCount + 1

    member this.ConnectCallCount = connectCallCount
    member this.AuthCallCount = authCallCount
