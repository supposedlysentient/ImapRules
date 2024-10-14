module Agent.Tests

open Expecto
open MailKit
open Agent
open Mocks

let msgs: IMessageSummary seq =
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
    |> Seq.map MockMessageSummary
    |> Seq.map (fun m -> upcast m)

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
    ]
