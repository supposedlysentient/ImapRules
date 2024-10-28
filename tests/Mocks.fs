module Mocks

open Foq
open MailKit
open Config
open Logging
open Client
open Checkpoint

let config = {
    server = "mail.example.com"
    port = 143
    sslOptions = SslOptions.None
    credential = System.Net.NetworkCredential ("mockuser", "mockpass")
    rulePath = []
    checkpointPath = ""
    logPath = None
    logConsole = true
    verbosity = Stream.Debug
}

module Agent =
    let inboxValidity = 333u

    let makeUids (ids: int seq) =
        [ for id in ids do UniqueId (inboxValidity, uint id) ]

    let makeMsgs (uids: UniqueId seq) =
        [ for uid in uids do
            Mock<IMessageSummary>()
                .Setup(fun m -> <@ m.UniqueId @>).Returns(uid)
                .Create()
        ]

    let filterMsgs (uids: UniqueId list) (msgs: IMessageSummary list) =
        msgs |> List.filter (fun msg -> List.contains msg.UniqueId uids)

    let makeLogger () =
        Mock<ILogger>()
            .SetupMethod(fun c -> <@ c.Log : obj -> unit @>).Returns(())
            .SetupMethod(fun c -> <@ c.Log : Stream -> (obj -> unit) @>).Returns(ignore)
            .Create()

    let makeClient
        (toFetch: IMessageSummary list)
        (fromQuery: UniqueId list)
        (uidNext: UniqueId) =
        Mock<IClient>()
            .SetupMethod(fun c -> <@ c.Fetch @>).Returns(toFetch)
            .SetupMethod(fun c -> <@ c.Search @>).Returns(fromQuery)
            .SetupMethod(fun c -> <@ c.Delete @>).Returns(())
            .SetupMethod(fun c -> <@ c.MoveTo @>).Returns(())
            .Setup(fun c -> <@ c.UidNext @>).Returns(uidNext)
            .Setup(fun c -> <@ c.UidValidity @>).Returns(inboxValidity)
            .Create()

    let makeCheckpoint (uidNext: UniqueId) =
        Mock<ICheckpoint>()
            .SetupMethod(fun cp -> <@ cp.Read @>).Returns(uidNext.Id)
            .Create()
