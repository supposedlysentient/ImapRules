module Grammar

open Lexicon


exception ParseError

type Requirement = string

type Require = Require of Requirement list

type AddressPart =
    | LocalPart
    | Domain
    | All

type MatchType =
    | Is
    | Contains
    | Matches

type Comparator = Comparator of string

type SizeQualifier =
    | Over
    | Under

type Header = string

// https://mailutils.org/manual/html_chapter/Sieve-Language.html
type Test =
    | Address of AddressPart option * MatchType option * Comparator option * Header list * string list
    | Envelope of AddressPart option * MatchType option * Comparator option * Header list * string list
    | Header of MatchType option * Comparator option * Header list * string list
    | Size of SizeQualifier * decimal
    | AllOf of Test list
    | AnyOf of Test list
    | Not of Test
    | Exists of Header list
    | True
    | False

type Address = string
type Folder = string
type RejectionMessage = string

type Action =
    | Reject of RejectionMessage
    | Redirect of Address
    | Keep
    | Discard
    | FileInto of Folder

type Conditional = Conditional of Test * Action list

type Control =
    | If of Conditional * Conditional list * Action list
    | Stop

type Command =
    | Require of Require
    | Action of Action
    | Control of Control

module private Production =
    let rec pStringList tokens =
        match tokens with
        | StringLiteral item :: tail -> [ item ], tail
        | SqBracketL :: SqBracketR :: tail -> [], tail
        | SqBracketL :: StringLiteral sHead :: tail ->
            let sTail, tail' = pStringListTail tail

            match tail' with
            | SqBracketR :: tail'' -> sHead :: sTail, tail''
            | _ -> [], tokens
        | _ -> [], tokens

    and pStringListTail tokens =
        match tokens with
        | Comma :: StringLiteral item :: tail ->
            match pStringListTail tail with
            | [], tail' -> [ item ], tail'
            | items, tail' -> item :: items, tail'

        | _ -> [], tokens

    and pTest tokens =
        match tokens with
        | (Token.Address | Token.Envelope | Token.Header as testType) :: tail ->
            let (addressPart: AddressPart option), matchType, comparator, tail' =
                pTestOptions None None None tail

            if testType = Token.Header && addressPart.IsSome then
                raise ParseError

            let headers, tail'' = pStringList tail'
            let keys, tail''' = pStringList tail''

            if headers = [] then
                raise ParseError

            if keys = [] then
                raise ParseError

            match addressPart, testType with
            | _, Token.Address -> Some(Test.Address(addressPart, matchType, comparator, headers, keys)), tail'''
            | _, Token.Envelope -> Some(Envelope(addressPart, matchType, comparator, headers, keys)), tail'''
            | None, Token.Header -> Some(Header(matchType, comparator, headers, keys)), tail'''
            | Some _, Token.Header -> raise ParseError
            | _ -> raise ParseError

        | Token.Size :: (Token.Over | Token.Under as sizeQual) :: NumLiteral limit :: tail ->
            let sizeQual' =
                if sizeQual = Token.Under then
                    SizeQualifier.Under
                else
                    SizeQualifier.Over

            Some(Size(sizeQual', limit)), tail

        | (Token.AllOf | Token.AnyOf as token) :: BracketL :: tail ->
            match pTest tail with
            | Some tHead', tail' ->
                let tTail, tail'' = pTestListTail tail'
                let tests = tHead' :: tTail

                match token with
                | Token.AnyOf -> Some(AnyOf tests), tail''
                | Token.AllOf -> Some(AllOf tests), tail''
                | _ -> raise ParseError
            | None, _ -> raise ParseError

        | Token.Not :: tail ->
            match pTest tail with
            | Some test, tail' -> Some(Not test), tail'
            | None, _ -> raise ParseError

        | Token.Exists :: tail ->
            match pStringList tail with
            | [], _ -> raise ParseError
            | sl, tail' -> Some(Exists sl), tail'

        | Token.True :: tail -> Some True, tail
        | Token.False :: tail -> Some False, tail
        | _ -> None, tokens

    and pTestOptions
        (addressPart: AddressPart option)
        (matchType: MatchType option)
        (comparator: Comparator option)
        tokens
        =
        match addressPart, matchType, comparator, tokens with
        | None, _, _, (Token.LocalPart | Token.Domain | Token.All as ap) :: tail ->
            let ap' =
                match ap with
                | Token.LocalPart -> LocalPart
                | Token.Domain -> Domain
                | Token.All -> All
                | _ -> raise ParseError

            pTestOptions (Some ap') matchType comparator tail
        | _, None, _, (Token.Is | Token.Contains | Token.Matches as mt) :: tail ->
            let mt' =
                match mt with
                | Token.Is -> Is
                | Token.Contains -> Contains
                | Token.Matches -> Matches
                | _ -> raise ParseError

            pTestOptions addressPart (Some mt') comparator tail
        | _, _, None, Token.Comparator :: StringLiteral sl :: tail ->
            let c' = Comparator sl
            pTestOptions addressPart matchType (Some c') tail
        | _, _, _, (Token.LocalPart | Token.Domain | Token.All | Token.Is | Token.Contains | Token.Matches) :: _ ->
            raise ParseError
        | _ -> addressPart, matchType, comparator, tokens

    and pTestListTail tokens =
        match tokens with
        | Comma :: tail ->
            match pTest tail with
            | Some tHead, tail ->
                let tTail, tail' = pTestListTail tail
                tHead :: tTail, tail'
            | _ -> [], tokens
        | _ -> [], tokens

    and pAction tokens =
        match tokens with
        | Token.Reject :: StringLiteral msg :: Semicolon :: tail -> Reject msg, tail
        | Token.Redirect :: StringLiteral address :: Semicolon :: tail -> Redirect address, tail
        | Token.Keep :: Semicolon :: tail -> Keep, tail
        | Token.Discard :: Semicolon :: tail -> Discard, tail
        | Token.FileInto :: StringLiteral folder :: Semicolon :: tail -> FileInto folder, tail
        | _ -> raise ParseError

    and pActionTail tokens =
        try
            let aHead, tail = pAction tokens
            let aTail, tail' = pActionTail tail
            aHead :: aTail, tail'
        with _ ->
            [], tokens

    and pActionBlock tokens =
        match tokens with
        | CBracketL :: tail ->
            match pActionTail tail with
            | actions, CBracketR :: tail' -> actions, tail'
            | _, _ -> raise ParseError
        | _ -> raise ParseError

    and pConditionalBlock tokens =
        match pTest tokens with
        | Some test, tail ->
            let actions, tail' = pActionBlock tail
            Some(Conditional(test, actions)), tail'
        | None, _ -> None, tokens

    and pElsifList tokens =
        match tokens with
        | Elsif :: tail ->
            match pConditionalBlock tail with
            | Some c, tail' ->
                let cTail, tail'' = pElsifList tail'
                c :: cTail, tail''
            | _ -> [], tokens
        | _ -> [], tokens

    and pIf tokens =
        match tokens with
        | Token.If :: tail ->
            match pConditionalBlock tail with
            | Some ifBlock, tail' ->
                match pElsifList tail' with
                | elsifBlocks, Token.Else :: tail'' ->
                    let elseActions, tail''' = pActionBlock tail''
                    Some(Control.If(ifBlock, elsifBlocks, elseActions)), tail'''
                | elsifBlocks, tail'' -> Some(Control.If(ifBlock, elsifBlocks, [])), tail''
            | None, _ -> raise ParseError
        | _ -> None, tokens

    and pControl tokens =
        match tokens with
        | Token.Stop :: Semicolon :: tail -> Some Stop, tail
        | Token.If :: _ ->
            match pIf tokens with
            | Some if', tail -> Some if', tail
            | _ -> None, tokens
        | _ -> None, tokens

    and pRequire tokens =
        match tokens with
        | Token.Require :: tail ->
            let sl, tail' = pStringList tail
            Some(Require.Require sl), tail'
        | _ -> None, tokens

    and pCommandList tokens =
        match pRequire tokens with
        | None, [] -> []
        | Some r, tail -> Command.Require r :: pCommandList tail
        | None, _ ->
            match pControl tokens with
            | Some ctl, tail -> Command.Control ctl :: pCommandList tail
            | None, _ ->
                try
                    let action, tail = pAction tokens
                    Command.Action action :: pCommandList tail
                with _ ->
                    raise ParseError

let parse (tokens: Token list) = Production.pCommandList tokens
