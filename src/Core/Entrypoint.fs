module Entrypoint

open Agent

type CommandLineOptions =
    { config: string
      daemonize: bool
      fetchOne: bool
      query: string }

    static member Default =
        { config = "server.json"
          daemonize = false
          fetchOne = false
          query = "" }

let printMessage (msg: MailKit.IMessageSummary) =
    let sep = "; "
    printfn $"Subject: {msg.Envelope.Subject}"
    printfn $"From:    {msg.Envelope.From |> Seq.map string |> String.concat sep}"
    printfn $"To:      {msg.Envelope.To |> Seq.map string |> String.concat sep}"
    printfn ""

let fetchOne config =
    let agent = new Agent(config)
    agent.FetchOne()

let query config rule =
    let agent = new Agent(config)
    agent.Query rule

let main args =
    let rec parseCommandLine options args =
        match args with
        | [] -> options
        | "--config" :: path :: tail -> parseCommandLine { options with config = path } tail
        | "--daemonize" :: tail -> parseCommandLine { options with daemonize = true } tail
        | "--fetch-one" :: tail -> parseCommandLine { options with fetchOne = true } tail
        | "--query" :: q :: tail when q.Length > 0 -> parseCommandLine { options with query = q } tail
        | s :: _ -> failwith $"bad argument: '{s}'"

    let options = args |> List.ofSeq |> parseCommandLine CommandLineOptions.Default

    let config = Config.read options.config

    match options with
    | { query = q } when q.Length > 0 ->
        for msg in query config q do
            printMessage msg
    | { fetchOne = true } -> fetchOne config |> printMessage
    | _ -> ()

    0
