module Rules

open System.IO
open MailKit
open Grammar
open Agent
open ImapRules.Sieve

let read (path: string) =
    let content = File.ReadAllText(path)
    parse content

let isMatch = Helpers.isMatch

let private act agent (action: Action) (msg: IMessageSummary) =
    match action with
    | Reject msg -> printfn $"rejecting with {msg}"
    | Redirect address -> printfn $"redirecting to {address}"
    | Keep -> ()
    | Discard -> printfn $"discarding"
    | FileInto folder -> printfn $"filing into {folder}"

let rec private process' agent (cmd: Command) (msg: IMessageSummary) =
    match cmd with
    | Require r -> printfn $"requiring {r}"
    | Action a -> act agent a msg
    | Control Stop -> printfn $"control stop" // TODO: Stop is an Action, not a Control
    | Control(If(ifBlock, elseIfBlocks, elseBlock)) ->
        let conditionals = ifBlock :: elseIfBlocks @ [ Conditional(True, elseBlock) ]

        let matchingConds =
            conditionals
            |> List.skipWhile (fun (Conditional(test, _)) -> isMatch test msg |> not)

        match matchingConds with
        | [] -> ()
        | firstMatch :: _ ->
            let (Conditional(_, actions)) = firstMatch
            actions |> List.iter (fun a -> act agent a msg)
    | Test t -> printfn $"test {t}"

let processMessage (agent: Agent) (rules: Command list) (msg: IMessageSummary) =
    rules |> List.iter (fun rule -> process' agent rule msg)
