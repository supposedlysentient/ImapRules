module Sieve.Tests

open Expecto
open FSharp.Core
open Lexicon
open Grammar

type TestCase = {
    input: string
    tokens: Token list
    ast: Command list
}

let testCases = [
    {
        input = "if address :is \"from\" \"daffy.duck@example.com\" {fileinto \"friends\";}"
        tokens = [
            Token.If
            Token.Address
            Token.Is
            StringLiteral "from"
            StringLiteral "daffy.duck@example.com"
            CBracketL
            Token.FileInto
            StringLiteral "friends"
            Semicolon
            CBracketR
        ]
        ast = [
            Control(
                If(
                    Conditional(
                        Test.Address(None, Some Is, None, [ "from" ], [ "daffy.duck@example.com" ]),
                        [ FileInto "friends" ]
                    ),
                    [],
                    []
                )
            )
        ]
    }
    {
        input = "if header :contains \"list-id\" \"<duck-hunting.example.com>\" {reject \"No violence, please\";}"
        tokens = [
            Token.If
            Token.Header
            Token.Contains
            StringLiteral "list-id"
            StringLiteral "<duck-hunting.example.com>"
            CBracketL
            Token.Reject
            StringLiteral "No violence, please"
            Semicolon
            CBracketR
        ]
        ast = [
            Control(
                If(
                    Conditional(
                        Header(Some Contains, None, [ "list-id" ], [ "<duck-hunting.example.com>" ]),
                        [ Reject "No violence, please" ]
                    ),
                    [],
                    []
                )
            )
        ]
    }
]

[<Tests>]
let tests =
    testList "Sieve" [
        for case in testCases do
            let name = case.input

            let assertion () =
                let actualTokens = lex case.input
                Expect.equal actualTokens case.tokens "Should lex into tokens"

                let ast = parse actualTokens
                Expect.equal ast case.ast "Should parse into AST"

            testCase name assertion
    ]
