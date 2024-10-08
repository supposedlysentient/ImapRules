module Agent.Tests

open Expecto
open Agent
open Mocks

[<Tests>]
let tests =
    testList "Core" [
        testCase "Connects over IMAP" (fun _ ->
            let client = new MockClient()
            let agent = new Agent(config, client)
            Expect.equal client.ConnectCallCount 1 "Should connect"
            Expect.equal client.AuthCallCount 1 "Should authenticate")
        testCase "Fetches one message" (fun _ ->
            let client = new MockClient()
            let agent = new Agent(config, client)
            let inbox = client.Inbox :?> MockFolder
            let msg = agent.FetchOne()
            Expect.equal inbox.FetchCallCount 1 "Should fetch")
    ]
