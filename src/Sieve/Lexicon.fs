module Lexicon

type Token =

    | BracketL
    | BracketR
    | SqBracketL
    | SqBracketR
    | CBracketL
    | CBracketR
    | Comma
    | Semicolon

    | Require

    | If
    | Elsif
    | Else

    | Address
    | Envelope
    | Header
    | Size
    | AllOf
    | AnyOf
    | Not
    | Exists
    | True
    | False

    | LocalPart
    | Domain
    | All

    | Is
    | Contains
    | Matches
    | Over
    | Under

    | Comparator

    | StringLiteral of string
    | NumLiteral of decimal

    | Reject
    | Redirect
    | Keep
    | Discard
    | FileInto
    | Stop

exception LexError of string * string * int


module Helpers =
    open System.Collections.Generic
    open FSharp.Reflection

    let whitespaceChars = Set([ ' '; '\t'; '\r'; '\n' ])
    let punctuationChars = Set([ '('; ')'; '['; ']'; '{'; '}'; ','; ';' ])
    let digitChars = Set([ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ])

    let sizeSuffixChars =
        Map [
            'k', 1024m
            'K', 1024m
            'm', 1048576m
            'M', 1048576m
            'g', 1073741824m
            'G', 1073741824m
        ]

    let isWhitespace (c: char) = Set.contains c whitespaceChars
    let isPunctuation (c: char) = Set.contains c punctuationChars
    let isDigit (c: char) = Set.contains c digitChars

    let isWordBreak (chars: char list) =
        match chars with
        | [] -> true
        | c :: _ when isWhitespace c -> true
        | _ -> false

    let flatten (chars: char seq) = Seq.rev chars |> System.String.Concat

    let textToToken = new Dictionary<string, Token>()
    let tokenToText = new Dictionary<Token, string>()

    let createToken (chars: char seq) =
        let s = flatten chars
        let success, token = textToToken.TryGetValue(s.ToLower())
        if success then token else StringLiteral s

    let tokenCases = FSharpType.GetUnionCases typeof<Token>

    let caseHasNoArgs (caseInfo: UnionCaseInfo) = caseInfo.GetFields().Length = 0
    let noArgCases = tokenCases |> Array.filter caseHasNoArgs

    for caseInfo in noArgCases do
        let case = FSharpValue.MakeUnion(caseInfo, [||]) :?> Token
        let name = caseInfo.Name.ToLower()

        let text =
            match case with
            | LocalPart
            | Domain
            | All
            | Is
            | Contains
            | Matches
            | Over
            | Under
            | Comparator -> $":{name}"

            | BracketL -> "("
            | BracketR -> ")"
            | SqBracketL -> "["
            | SqBracketR -> "]"
            | CBracketL -> "{"
            | CBracketR -> "}"
            | Comma -> ","
            | Semicolon -> ";"

            | _ -> name

        textToToken[text] <- case
        tokenToText[case] <- text

open Helpers

let lex (s: string) =
    let rec scanString (buffer: char list) (position: int) (chars: char list) =
        let pos' = position + 1
        let pos'' = position + 2

        match buffer, chars with
        | _, [] -> raise (LexError("Unterminated string", s, pos'))
        | [], '"' :: tail -> scanString buffer pos' tail
        | _, '"' :: tail -> flatten buffer, pos'', tail
        | _, '\\' :: '"' :: tail -> scanString ('"' :: buffer) pos'' tail
        | _, '\\' :: '\\' :: tail -> scanString ('\\' :: buffer) pos'' tail
        | _, c :: tail -> scanString (c :: buffer) pos' tail

    let rec scanNumber
        (buffer: char list)
        (position: int)
        (hasDecimalPoint: bool)
        (multiplier: decimal)
        (chars: char list)
        =
        let pos' = position + 1

        match hasDecimalPoint, chars with
        | _, c :: tail when isDigit c -> scanNumber (c :: buffer) pos' hasDecimalPoint multiplier tail
        | false, ('.' as c) :: d :: tail when isDigit d ->
            scanNumber (c :: buffer) pos' hasDecimalPoint multiplier (d :: tail)
        | _, '.' :: _ -> raise (LexError("Illegal number format", s, pos'))
        | _, c :: tail when sizeSuffixChars.ContainsKey c && isWordBreak tail ->
            let mult' = sizeSuffixChars[c]
            scanNumber buffer pos' hasDecimalPoint mult' tail
        | _, tail when isWordBreak tail ->
            let s = flatten buffer
            let d = decimal s
            d * multiplier, pos', tail
        | _, _ -> raise (LexError("Illegal number format", s, pos'))

    let rec scan (buffer: char list) (position: int) (chars: char list) =
        let pos' = position + 1

        match buffer, chars with
        | [], [] -> []
        | _, [] -> [ createToken buffer ]
        | [], c :: tail when isPunctuation c -> createToken [ c ] :: scan [] pos' tail
        | _, c :: tail when isPunctuation c -> createToken buffer :: createToken [ c ] :: scan [] pos' tail
        | [], c :: tail when isWhitespace c -> scan [] pos' tail
        | _, c :: tail when isWhitespace c -> createToken buffer :: scan [] pos' tail
        | [], '"' :: _ ->
            let s, pos'', tail' = scanString [] 0 chars
            StringLiteral s :: scan [] pos'' tail'
        | _, '"' :: _ -> raise (LexError("Illegal quote", s, pos'))
        | [], d :: _ when isDigit d ->
            let num, pos'', tail' = scanNumber [] pos' false 1m chars
            NumLiteral num :: scan [] pos'' tail'
        | _, c :: tail -> scan (c :: buffer) pos' tail

    s |> List.ofSeq |> scan [] 0
