module Agent.Tests

open Expecto
open MailKit
open Config
open Agent
open Mocks

let bareMsg = MockMessageData.Default
let msgs =
    [
        {
            bareMsg with
                To = [ "primary@inbox.mock" ]
                From = [ "buddy@my.friend" ]
                Subject = "Holiday plans"
        }
        {
            bareMsg with
                To = [ "primary@inbox.mock" ]
                From = [ "buddy@my.friend" ]
                Subject = "Party!!!?!?!?"
        }
        {
            bareMsg with
                To = [ "primary@inbox.mock"; "brother@foo.bar"; "sister@example.com" ]
                From = [ "mother@my.family" ]
                Subject = "Christmas"
        }
        {
            bareMsg with
                To = [ "primary@inbox.mock" ]
                From = [ "campaign@spyware.bastards" ]
                ReplyTo = [ "sales@crap.product" ]
                Subject = "New bullsh*t launch!"
        }
        {
            bareMsg with
                To = [ "alias@inbox.mock" ]
                From = [ "distant.acquaintance@somewhere.far" ]
                Subject = "Juicy gossip"
                Size = Some 500000u
        }
        {
            bareMsg with
                To = [ "primary@inbox.mock" ]
                From = [ "father@my.family" ]
                Subject = "Very disappointed"
        }
    ]
    |> List.map (fun m -> MockMessageSummary m :> IMessageSummary)

type Expectation =
    | NoMessage
    | Message of IMessageSummary
    | Messages of IMessageSummary list
    | Error

type queryCase = {
    name: string
    query: string
    expected: Expectation
}

let queryTheories = [
    {
        name = "finds by From address"
        query =
            """
            address "from" "buddy@my.friend"
            """
        expected = Messages msgs[0..1]
    }
    {
        name = "is case-insensitive"
        query =
            """
            aDdrEss "fRoM" "bUDdY@my.FrIeNd"
            """
        expected = Messages msgs[0..1]
    }
    {
        name = "finds nothing when From address does not match"
        query =
            """
            address "from" "does@not.exist"
            """
        expected = NoMessage
    }
    {
        name = "finds by entire address"
        query =
            """
            address :all "from" "buddy@my.friend"
            """
        expected = Messages msgs[0..1]
    }

    {
        name = "finds by local part"
        query =
            """
            address :localpart "from" "buddy"
            """
        expected = Messages msgs[0..1]
    }
    {
        name = "finds by domain"
        query =
            """
            address :domain "from" "my.friend"
            """
        expected = Messages msgs[0..1]
    }
    {
        name = "finds by To address"
        query =
            """
            address "to" "brother@foo.bar"
            """
        expected = Message msgs[2]
    }
    {
        name = "finds by list of From addresses"
        query =
            """
            address "from" [ "brother@foo.bar", "sister@example.com", "mother@my.family" ]
            """
        expected = Message msgs[2]
    }
    {
        name = "finds by list of local part of To, From, CC, or BCC addresses"
        query =
            """
            address :localpart [ "to", "from", "cc", "bcc" ] [ "brother", "sister", "mother", "father" ]
            """
        expected = Messages [ msgs[2]; msgs[5] ]
    }
    {
        name = "finds nothing when list of local part of To, CC, or BCC addresses does not match"
        query =
            """
            address :localpart [ "to", "cc", "bcc" ] [ "foo", "bar" ]
            """
        expected = NoMessage
    }
    {
        name = "finds by Subject"
        query =
            """
            header "subject" "Juicy gossip"
            """
        expected = Message msgs[4]
    }
    {
        name = "is case-insensitive in Subject"
        query =
            """
            header "subject" "jUICY GOSSIP"
            """
        expected = Message msgs[4]
    }
    {
        name = "finds by Size"
        query =
            """
            size :over 200k
            """
        expected = Message msgs[4]
    }
    {
        name = "finds nothing when Size does not match"
        query =
            """
            size :over 900k
            """
        expected = NoMessage
    }
]

let makeClient (config: Config) (msgs: IMessageSummary seq) =
    let client = new MockClient ()
    let checkpoint = new MockCheckpoint ()
    let agent = new Agent (config, client, checkpoint)
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
            let msg = agent.FetchOne ()
            Expect.equal inbox.FetchCallCount 1 "Should fetch"
            Expect.equal msg (Seq.head msgs) "Should return first message")
        ptestList // WIP
            "Queries"
            (queryTheories
             |> List.map (fun case ->
                 testCase case.name (fun _ ->
                     let agent, _, _ = makeClient config msgs

                     match case.expected with
                     | Error ->
                         Expect.throwsT<Grammar.ParseError>
                             (fun _ -> (*agent.Query*) case.query |> ignore)
                             "Should raise parse error"
                     | _ ->
                         let expected' =
                             match case.expected with
                             | Message e -> [ e ]
                             | Messages e -> e
                             | _ -> []

                         let actual = (*agent.Query case.query*) msgs

                         Expect.equal
                             actual
                             expected'
                             $"Should find {expected'.Length} message(s) with '{case.query.Trim()}'")))
    ]
