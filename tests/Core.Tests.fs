module Core.Tests

open Expecto
open FSharp.Core

type TestCase = { input: string; tokens: Token list }

let testCases = [
    {
        input = "if address :is \"from\" \"daffy.duck@example.com\" {fileinto \"friends\";}"
        tokens = [
            If
            Address
            Is
            StringLiteral "from"
            StringLiteral "daffy.duck@example.com"
            CBracketL
            FileInto
            StringLiteral "friends"
            Semicolon
            CBracketR
        ]
    }
    {
        input = "if header :contains \"list-id\" \"<duck-hunting.example.com>\" {reject \"No violence, please\";}"
        tokens = [
            If
            Header
            Contains
            StringLiteral "list-id"
            StringLiteral "<duck-hunting.example.com>"
            CBracketL
            Reject
            StringLiteral "No violence, please"
            Semicolon
            CBracketR
        ]
    }
]

let tests =
    testList "Sieve" [
        for case in testCases do
            let name = case.input

            let assertion () =
                let actualTokens = lex case.input
                Expect.equal actualTokens case.tokens "Should lex into tokens"

            testCase name assertion
    ]

[<EntryPoint>]
let main _ = runTestsWithCLIArgs [] [||] tests
