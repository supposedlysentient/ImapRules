module Daemon

open MailKit
open Grammar
open Config
open Logging
open Agent
open Rules

let batchSize = 10u

let run (config: Config) (rules: Command list) (date: System.DateTimeOffset option) =
    let logger = makeLogger config
    logger.Log Stream.Output "Agent starting"

    let mutable keepRunning = true
    System.Console.CancelKeyPress.Add (
        fun _ ->
            logger.Log Stream.Output "Agent stopping"
            keepRunning <- false)

    let agent = new Agent (config, logger)

    let uids =
        match date with
        | Some date -> agent.GetUidsSince date
        | None -> agent.GetUidsSinceCheckpoint () |> List.truncate (int batchSize)

    let rec run' (uids: UniqueId list) =
        let msgs = agent.Fetch uids
        msgs
        |> List.sortBy (fun msg -> msg.UniqueId.Id) // for checkpoint reasons
        |> List.iter (fun msg ->
            agent.WriteCheckpoint msg.UniqueId
            processMessage agent rules msg)

        let uids' =
            match msgs with
            | [] -> uids
            | _ ->
                let lastMsg = msgs |> List.sortBy (fun m -> m.UniqueId.Id) |> List.last
                let next = lastMsg.UniqueId.Id + 1u

                [ next .. (next + batchSize) ]
                |> List.map (fun id -> UniqueId(lastMsg.UniqueId.Validity, id))

        System.Threading.Thread.Sleep 2000

        if keepRunning then run' uids'

    run' uids

    logger.Log Stream.Output "Agent stopping"
