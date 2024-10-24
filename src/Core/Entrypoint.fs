module Entrypoint

open Agent

type CommandLineOptions = {
    config: string
    daemonize: bool
    fetch: uint16 option
    since: System.DateTimeOffset option
    query: string
    testRules: string
} with

    static member Default = {
        config = "server.json"
        daemonize = false
        fetch = None
        since = None
        query = ""
        testRules = ""
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

let fetchSince config date =
    let agent = new Agent(config)
    agent.FetchSince date

let testRules config =
    config.rulePath
    |> List.map Rules.read
    |> List.iter (fun rule -> printfn "%A" rule)

let query config rule =
    let agent = new Agent(config)
    agent.Query rule

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
        | "--query" :: q :: tail when q.Length > 0 -> parseCommandLine { options with query = q } tail
        | "--test-rules" :: path :: tail when path.Length > 0 -> parseCommandLine { options with testRules = path } tail
        | s :: _ -> failwith $"bad argument: '{s}'"

    let options = args |> List.ofSeq |> parseCommandLine CommandLineOptions.Default

    let config = Config.read options.config

    match options with
    | { testRules = path } when path.Length > 0 -> testRules config
    | { query = q } when q.Length > 0 -> query config q |> List.iter printMessage
    | { fetch = Some count } -> fetch config count |> List.iter printMessage
    | { since = Some date } -> fetchSince config date |> List.iter printMessage
    | _ -> ()

    0
