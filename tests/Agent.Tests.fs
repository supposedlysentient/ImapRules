module Agent.Tests

open Expecto
open MailKit
open Agent
open Mocks

let msgs =
    [
        {
            MockMessageData.Default with
                To = [ "primary@inbox.mock" ]
                From = [ "buddy@my.friend" ]
                Subject = "Holiday plans"
        }
        {
            MockMessageData.Default with
                To = [ "primary@inbox.mock" ]
                From = [ "buddy@my.friend" ]
                Subject = "Party!!!?!?!?"
        }
        {
            MockMessageData.Default with
                To = [ "primary@inbox.mock"; "brother@foo.bar"; "sister@example.com" ]
                From = [ "mother@my.family" ]
                Subject = "Christmas"
        }
        {
            MockMessageData.Default with
                To = [ "primary@inbox.mock" ]
                From = [ "campaign@spyware.bastards" ]
                ReplyTo = [ "sales@crap.product" ]
                Subject = "New bullsh*t launch!"
        }
        {
            MockMessageData.Default with
                To = [ "alias@inbox.mock" ]
                From = [ "distant.acquaintance@somewhere.far" ]
                Subject = "Juicy gossip"
        }
        {
            MockMessageData.Default with
                To = [ "primary@inbox.mock" ]
                From = [ "father@my.family" ]
                Subject = "Very disappointed"
        }
    ]
    |> List.map (fun m -> MockMessageSummary m :> IMessageSummary)

let queryTheories = [
    ("address \"from\" \"buddy@my.friend\"", msgs[0..1])
    ("address \"to\" \"brother@foo.bar\"", [ msgs[2] ])
]

let makeClient (config: Config) (msgs: IMessageSummary seq) =
    let client = new MockClient()
    let agent = new Agent(config, client)
    let inbox = client.Inbox :?> MockFolder
    inbox.Messages <- msgs
    agent, client, inbox

[<Tests>]
let tests =
    testList "Core" [
        testCase "Connects over IMAP" (fun _ ->
            let agent, client, _ = makeClient config []
            Expect.equal client.ConnectCallCount 1 "Should connect"
            Expect.equal client.AuthCallCount 1 "Should authenticate")
        testCase "Fetches one message" (fun _ ->
            let agent, client, inbox = makeClient config msgs
            let msg = agent.FetchOne()
            Expect.equal inbox.FetchCallCount 1 "Should fetch"
            Expect.equal msg (Seq.head msgs) "Should return first message")
        testList
            "Queries"
            (queryTheories
             |> List.map (fun (query, expected) ->
                 testCase query (fun _ ->
                     let agent, _, _ = makeClient config msgs
                     let actual = agent.Query query
                     Expect.equal actual expected $"Should find {expected.Length} message(s)")))
    ]
