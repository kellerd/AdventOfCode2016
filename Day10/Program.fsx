#r "bin/Debug/Library.dll"

open Advent.Library
open System.Text.RegularExpressions
open System.Collections.Generic

type Id = | BotId of int
          | OutputId of int

type BotBin = {Id : Id 
               Low :  Id option 
               High : Id option 
               Bin : int list}
type Msg = 
    | Set of int
    | Give of Id * Id
    | Get of AsyncReplyChannel<BotBin>

type SuperMsg = 
    | Value of int * Id
    | Distribute of Id * Id * Id
    | CreateAgent of (Id -> MailboxProcessor<Msg>)


let file = "\input.txt"

let bots : Dictionary<Id,MailboxProcessor<Msg>> = new Dictionary<_,_>()  
let output : Dictionary<Id,MailboxProcessor<Msg>> = new Dictionary<_,_>()
let superVisor = MailboxProcessor.Start(fun inbox -> 
        let createOrGetAgent init (dict:Dictionary<Id,MailboxProcessor<_>>) id = 
            if not (dict.ContainsKey id) then
                let newAgent = init id;
                dict.Add(id,newAgent)
                newAgent
            else dict.Item id
        let superVisorUpdate init msg =
            let dict id = match id with | BotId _ -> createOrGetAgent init bots id | OutputId _ -> createOrGetAgent init output id
            match msg with
            | Value (v,id) -> (dict id).Post(Set(v))
            | Distribute (id,lowId,highId) -> (dict id).Post(Give(lowId,highId))
            | _ -> ()
        let rec messageLoop createFunc = async{
            // read a message
            let! msg = inbox.Receive()
            // do the core logic
            match createFunc, msg with
            | None,CreateAgent(f) -> return! messageLoop (Some f)
            | None,_ -> failwith "Not ready"
            | Some(f),msg -> 
                do superVisorUpdate f msg
                return! messageLoop createFunc
        }
        messageLoop None
    )

let agent id = MailboxProcessor.Start(fun inbox -> 
    let updateState bot msg = 
        let update = function
            | Set (value) -> {bot with Bin = (value::bot.Bin |> List.distinct |> List.sort)}
            | Give (lowId,highId) -> {bot with Low = Some lowId; High = Some highId}
            | Get repl -> repl.Reply(bot);bot
        let newState = update msg
        match newState.Low, newState.High,newState.Bin with
        | (Some id),(Some highId),[lowVal;highVal]-> 
            superVisor.Post (Value (lowVal,id)); superVisor.Post(Value (highVal,highId));
            newState;
        | _ -> newState
    // the message processing function
    let rec messageLoop bot = async{
        // read a message
        let! msg = inbox.Receive()
        // do the core logic
        let newState = updateState bot msg
        return! messageLoop newState 
        // loop to top
        }

    // start the loop 
    messageLoop {Id = id
                 Low = None
                 High =None; Bin = []}
)

superVisor.Post (CreateAgent(agent))

open System.Text.RegularExpressions

let (|Match|_|) (pat:string) (inp:string) =
    let m = Regex.Match(inp, pat) in
    if m.Success
    then Some (List.tail [ for g in m.Groups -> g.Value ])
    else None



let parse (s:string) = 
    match s with
    | Match "value (\d+) goes to bot (\d+)" [v;id] -> Value(v |> int,id |> int |> BotId )
    | Match "value (\d+) goes to output (\d+)" [v;id] -> Value(v |> int,id |> int |> OutputId )
    | Match "bot (\d+) gives low to bot (\d+) and high to bot (\d+)" [id;idl;idh] -> Distribute(id |> int |> BotId, idl |> int |> BotId, idh |> int |> BotId)
    | Match "bot (\d+) gives low to bot (\d+) and high to output (\d+)" [id;idl;idh] -> Distribute(id |> int |> BotId, idl |> int |> BotId, idh |> int |> OutputId)
    | Match "bot (\d+) gives low to output (\d+) and high to bot (\d+)" [id;idl;idh] -> Distribute(id |> int |> BotId, idl |> int |> OutputId, idh |> int |> BotId)
    | Match "bot (\d+) gives low to output (\d+) and high to output (\d+)" [id;idl;idh] -> Distribute(id |> int |> BotId, idl |> int |> OutputId, idh |> int |> OutputId)
    | s -> failwith <| sprintf "Whoops %s" s

#time
System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + file)
|> Array.map (parse)
|> Array.iter (superVisor.Post)

bots.Values |> Seq.map (fun agent -> agent.PostAndReply(Get)) |> Seq.filter(function {Bin=[low;high] } when low = 17 && high=61 -> true | _ -> false) |> Seq.iter (printfn "%A")
output.Values |> Seq.map (fun agent -> agent.PostAndReply(Get)) |> Seq.filter(function {Id=OutputId 0} | {Id=OutputId 1} | {Id=OutputId 2} -> true | _ -> false ) |> Seq.map (fun {Bin=bin} -> bin |> List.head) |> Seq.fold (*) 1