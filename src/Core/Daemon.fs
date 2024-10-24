module Daemon

open MailKit
open Grammar
open Agent
open Rules

let run (agent: Agent) (rules: Command list) (date: System.DateTimeOffset) =
    let uids = agent.GetUidsSince date

    let rec run' (uids: UniqueId list) =
        let msgs = agent.Fetch uids
        msgs |> List.iter (fun msg -> processMessage agent rules msg)

        let uids' =
            match msgs with
            | [] -> uids
            | _ ->
                let lastMsg = msgs |> List.sortBy (fun m -> m.UniqueId.Id) |> List.last
                let next = lastMsg.UniqueId.Id + 1u

                [ next .. (next + 10u) ]
                |> List.map (fun id -> UniqueId(lastMsg.UniqueId.Validity, id))

        System.Threading.Thread.Sleep 2000

        run' uids'

    run' uids
