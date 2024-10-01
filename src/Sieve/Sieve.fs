namespace ImapRules

module Sieve =
    let parse (s: string) = s |> Lexicon.lex |> Grammar.parse
