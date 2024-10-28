module Agent.Tests

open Expecto
open Foq
open MailKit
open MailKit.Search
open Agent
open Mocks
open Mocks.Agent

module Client =
    open System.Collections.Generic
    open System.Text
    open System.Net
    open System.Threading
    open MailKit.Net.Imap
    open Client

    type SslOptions = Security.SecureSocketOptions

    type ConnectSig = string * int * SslOptions * CancellationToken -> unit
    type AuthSig = Encoding * ICredentials * CancellationToken -> unit
    type FolderSig = string * CancellationToken -> IMailFolder

    type OpenSig = FolderAccess * CancellationToken -> FolderAccess
    type FetchSig = IList<UniqueId> * IFetchRequest * CancellationToken -> IList<IMessageSummary>
    type MoveSig = UniqueId * IMailFolder * CancellationToken -> System.Nullable<UniqueId>
    type StoreSig = UniqueId * IStoreFlagsRequest * CancellationToken -> bool

    let uid = UniqueId (1u, 1u)

    let makeFolder () =
        Mock<IMailFolder>()
            .Setup(fun f -> <@ f.IsOpen @>).Returns(false)
            .SetupMethod(fun f -> <@ f.Open : OpenSig @>).Returns(FolderAccess.ReadWrite)
            .SetupMethod(fun f -> <@ f.Fetch : FetchSig @>).Returns(new List<IMessageSummary>())
            .SetupMethod(fun f -> <@ f.MoveTo : MoveSig @>).Returns(uid)
            .SetupMethod(fun f -> <@ f.Store : StoreSig @>).Returns(true)
            .SetupMethod(fun f -> <@ f.Expunge : CancellationToken -> unit @>).Returns(())
            .Create()

    let makeImapClient (inbox: IMailFolder) =
        Mock<IImapClient>()
            .Setup(fun c -> <@ c.IsConnected @>).Returns(false)
            .SetupMethod(fun c -> <@ c.Connect : ConnectSig @>).Returns(())
            .Setup(fun c -> <@ c.IsAuthenticated @>).Returns(false)
            .SetupMethod(fun c -> <@ c.Authenticate : AuthSig @>).Returns(())
            .Setup(fun c -> <@ c.Inbox @>).Returns(inbox)
            .SetupMethod(fun c -> <@ c.GetFolder : FolderSig @>).Returns(makeFolder ())
            .Create()

    let makeClient imapClient : IClient =
        new Client (config, makeLogger (), imapClient)

    [<Tests>]
    let tests =
        testList "Client" [
            test "connects" {
                let inbox = makeFolder ()
                let imapClient = makeImapClient inbox
                let client = makeClient imapClient

                Mock.Expect(<@ imapClient.IsConnected @>, Times.Once)
                Mock.Expect(<@ imapClient.Connect (
                    config.server,
                    config.port,
                    config.sslOptions) @>, Times.Once)
                Mock.Expect(<@ imapClient.IsAuthenticated @>, Times.Once)
                Mock.Expect(<@ imapClient.Authenticate (
                    Encoding.UTF8,
                    It.Is<ICredentials>(fun cred -> cred :?> NetworkCredential = config.credential)
                ) @>, Times.Once)

                client.Fetch [] |> ignore

                Mock.VerifyAll(imapClient)
            }
            test "opens inbox" {
                let inbox = makeFolder ()
                let imapClient = makeImapClient inbox
                let client = makeClient imapClient

                Mock.Expect(<@ inbox.IsOpen @>, Times.Once)
                Mock.Expect(<@ inbox.Open (It.Is(
                    fun (access: FolderAccess) -> access = FolderAccess.ReadWrite
                )) @>, Times.Once)

                client.Fetch [] |> ignore

                Mock.VerifyAll(inbox)
            }
            test "moves message" {
                let inbox = makeFolder ()
                let imapClient = makeImapClient inbox
                let client = makeClient imapClient
                let folderName = "foo"

                Mock.Expect(<@ imapClient.GetFolder folderName @>, Times.Once)
                Mock.Expect(<@ inbox.Open (It.Is(
                    fun (access: FolderAccess) -> access = FolderAccess.ReadWrite
                )) @>, Times.Once)

                client.MoveTo (uid, folderName)

                Mock.VerifyAll(imapClient)
                Mock.VerifyAll(inbox)
            }
            test "deletes message" {
                let inbox = makeFolder ()
                let imapClient = makeImapClient inbox
                let client = makeClient imapClient
                let deletedFlag = StoreFlagsRequest (StoreAction.Add, MessageFlags.Deleted)

                Mock.Expect(<@ inbox.Store (uid, It.Is<IStoreFlagsRequest>(
                    fun request -> request.Action = deletedFlag.Action && request.Flags = deletedFlag.Flags
                )) @>, Times.Once)
                Mock.Expect(<@ inbox.Expunge @>, Times.Once)

                client.Delete uid

                Mock.VerifyAll(inbox)
            }
        ]

module Fetch =
    let uidNext = UniqueId (inboxValidity, 666u)

    let uids = makeUids [ 1 ]
    let msgs = uids |> makeMsgs
    let makeClient msgs =
        Mocks.Agent.makeClient msgs uids uidNext

    let makeAgent msgs =
        new Agent(
            config,
            makeLogger (),
            makeClient msgs,
            makeCheckpoint uidNext
        )

    let date = System.DateTimeOffset.Parse("1999-12-31T23:59Z")

    type Fixture = {
        uids: UniqueId list
        msgs: IMessageSummary list
        logger: Logging.ILogger
        client: Client.IClient
        checkpoint: Checkpoint.ICheckpoint
        agent: Agent
    }

    let setup ids =
        let uids = makeUids ids
        let msgs = uids |> makeMsgs
        let logger = makeLogger ()
        let client = makeClient msgs
        let cp = makeCheckpoint uidNext
        {
            uids = uids
            msgs = msgs
            logger = logger
            client = client
            checkpoint = cp
            agent = new Agent(config, logger, client, cp)
        }


    [<Tests>]
    let tests =
        testList "Fetch" [
            testTheory "by count" [ []; [ 1 ]; [ 1 .. 5 ] ] (
                fun ids ->
                    let count = uint ids.Length
                    let fixture = setup ids
                    let actual = fixture.agent.Fetch count
                    Expect.equal actual.Length ids.Length $"should fetch {count} message(s)"
                    Expect.equal actual fixture.msgs $"should not change message order")

            testTheory "by uid" [ []; [ 1 ]; [ 1 .. 5 ] ] (
                fun ids ->
                    let count = uint ids.Length
                    let fixture = setup ids
                    let actual = fixture.agent.Fetch fixture.uids
                    Expect.equal actual.Length ids.Length $"should fetch {count} message(s)"
                    Expect.equal actual fixture.msgs $"should not change message order")

            test "since date" {
                let fixture = setup [ 1 .. 3 ]
                let actual = fixture.agent.FetchSince date
                Expect.equal actual fixture.msgs $"should fetch messages"

                let expected = SearchQuery.SentSince date.Date
                let queryTest (arg: SearchQuery) =
                    arg.Term = expected.Term &&
                    (arg :?> DateSearchQuery).Date = expected.Date
                Mock.Verify(<@ fixture.client.Search(It.Is(queryTest)) @>)
            }

            test "since checkpoint" {
                let fixture = setup [ 1 .. 3 ]
                let actual = fixture.agent.FetchSinceCheckpoint ()
                Expect.equal actual fixture.msgs $"should fetch messages"
                Mock.Verify(<@ fixture.checkpoint.Read () @>, Times.Once)
            }
        ]


[<Tests>]
let tests =
    testList "Agent" [
        Client.tests
        Fetch.tests
    ]
