module Entrypoint

open Agent

type Config = Config.Config

type CommandLineOptions = {
    config: string
    daemonize: bool
    fetch: uint16 option
    since: System.DateTimeOffset option
    query: string option
    validate: bool
    validateRules: bool
}

let defaultOptions = {
    config = "server.json"
    daemonize = false
    fetch = None
    since = None
    query = None
    validate = false
    validateRules = false
}

let printMessage (msg: MailKit.IMessageSummary) =
    let sep = "; "
    printfn $"Subject: {msg.Envelope.Subject}"
    printfn $"From:    {msg.Envelope.From |> Seq.map string |> String.concat sep}"
    printfn $"To:      {msg.Envelope.To |> Seq.map string |> String.concat sep}"
    printfn $"At:      {msg.Envelope.Date}"
    printfn ""

let fetch config count =
    let agent = new Agent(config)

    match count with
    | 0us -> []
    | 1us -> [ agent.FetchOne() ]
    | _ -> count |> int |> agent.Fetch

let fetchSince (config: Config) date =
    let agent = new Agent(config)
    agent.FetchSince date

let runAsDaemon (config: Config) date =
    // let agent = new Agent(config)
    let rules = config.rulePath |> List.map Rules.read |> List.concat
    Daemon.run config rules date

let validateRules (config: Config) =
    config.rulePath
    |> List.map Rules.read
    |> List.iter (fun rule -> printfn "%A" rule)

let validate (config: Config) query =
    let tokens = Lexicon.lex query
    printfn "%A" tokens
    Grammar.parse tokens
    |> List.iter (fun rule -> printfn "%A" rule)

let query config count (rule: string) =
    let agent = new Agent(config)
    let sieveCommands = ImapRules.Sieve.parse rule
    let test =
        match sieveCommands with
        | Grammar.Test t :: [] -> t
        | _ -> failwith "Sieve script was not a bare conditional test"
    let msgs = count |> int |> agent.Fetch
    msgs |> List.filter (Rules.isMatch test)

let main args =
    let rec parseCommandLine options args =
        match args with
        | [] -> options
        | "--config" :: path :: tail -> parseCommandLine { options with config = path } tail
        | "--daemonize" :: tail -> parseCommandLine { options with daemonize = true } tail
        | "--fetch" :: count :: tail ->
            let count' = System.UInt16.Parse(count)
            parseCommandLine { options with fetch = Some count' } tail
        | "--since" :: date :: tail ->
            let date' = System.DateTimeOffset.Parse(date)
            parseCommandLine { options with since = Some date' } tail
        | "--query" :: q :: tail when q.Length > 0 -> parseCommandLine { options with query = Some q } tail
        | "--validate-rules" :: tail -> parseCommandLine { options with validateRules = true } tail
        | "--validate" :: tail -> parseCommandLine { options with validate = true } tail
        | s :: _ -> failwith $"bad argument: '{s}'"

    let options = args |> List.ofSeq |> parseCommandLine defaultOptions

    let config = Config.read options.config

    match options with
    | { daemonize = true; since = date } -> runAsDaemon config date
    | { validateRules = true } -> validateRules config
    | { validate = true; query = (Some q) } -> validate config q
    | { query = (Some q); fetch = Some count } -> query config count q |> List.iter printMessage
    | { query = (Some q); fetch = None } -> query config 10us q |> List.iter printMessage
    | { fetch = Some count } -> fetch config count |> List.iter printMessage
    | { since = Some date } -> fetchSince config date |> List.iter printMessage
    | _ -> ()

    0
