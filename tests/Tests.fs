open Expecto

let tests = testList "All tests" [ Sieve.Tests.tests; Agent.Tests.tests ]

[<EntryPoint>]
let main _ = runTestsWithCLIArgs [] [||] tests
