open System.IO
open System
open System.Numerics
#r "bin/Debug/Library.dll"
open Advent.Library
#r "../packages/FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"
open FSharp.Collections.ParallelSeq

let (|REG|_|) = function
   | "a" -> Some 0
   | "b" -> Some 1
   | "c" -> Some 2
   | "d" -> Some 3
   | _ -> None

type Value = Register of string | Value of int
type Instr = | Copy of Value * Value | Inc of Value | Dec of Value | Jump of Value * Value | Toggle of Value | Out of Value
let parse (s:string) = 
    match s with
    | Prefix "cpy " (TakeWhile ' ' ((Int d), Prefix " " (register))) -> Copy(Value d,Register register)
    | Prefix "cpy " (TakeWhile ' ' (register1, Prefix " " (register2))) -> Copy(Register register1,Register register2)
    | Prefix "inc " register -> Inc(Register register)
    | Prefix "dec " register -> Dec(Register register)
    | Prefix "jnz " (TakeWhile ' ' ((Int d), Prefix " " (Int instructions) )) -> Jump(Value d,          Value instructions)
    | Prefix "jnz " (TakeWhile ' ' (register1, Prefix " " (Int instructions))) -> Jump(Register register1,Value instructions)
    | Prefix "jnz " (TakeWhile ' ' ((Int d), Prefix " " (register2))) -> Jump(Value d,Register register2)
    | Prefix "jnz " (TakeWhile ' ' (register1, Prefix " " (register2))) -> Jump(Register register1,Register register2)
    | Prefix "tgl " (Int d) -> Toggle(Value(d))
    | Prefix "tgl " register -> Toggle(Register(register))
    | Prefix "out " (Int d) -> Out(Value(d))
    | Prefix "out " register -> Out(Register(register))
    | x -> failwith <| sprintf "Couldn't parse %s" x
let toggle  = function
    | Copy (v,v2) -> Jump(v,v2)
    | Inc (v) -> Dec(v) 
    | Out(v) | Dec (v) | Toggle(v) -> Inc(v)
    | Jump (v,i) -> Copy(v,i)
let doWork registers instructions  = seq {
    let (|RegVal|_|) = function
        | Register (REG ind) -> Some (Array.item ind registers)
        | _ -> None
    let (|AnyVal|_|) = function
        | Register (REG ind) -> Some (Array.item ind registers)
        | Value (d) -> Some d
        | _ -> None
    let (|Multiply|_|) (instructions:_[]) start  =
        match instructions |> Array.skip start |> Array.truncate 6 with
        | [| Copy(AnyVal(b),Register c)
             Inc(Register a)
             Dec(Register c')
             Jump(Register c'',AnyVal -2)
             Dec(Register d)
             Jump(Register d' as dreg, AnyVal -5) |]
            when c = c' && c = c'' && d = d' && d <> a && d <> c
            -> dreg |> function RegVal d -> (a,c,d',(b * d)) |> Some | _ -> None
        | _ -> None
    let (|Instr|) (instructions:_[]) start =
        Array.item start instructions
    while registers.[4] < (Array.length instructions) do
        let instrNumber = registers.[4] |> int 
        match instrNumber with
        | Multiply instructions (REG a,REG c,REG d,result) -> registers.[a] <- registers.[a] + result; registers.[c] <- 0; registers.[d] <- 0; registers.[4] <- registers.[4] + 6
        | Instr instructions (Copy(Value d, Register(REG ind)) )
        | Instr instructions (Copy(RegVal d, Register(REG ind))) -> registers.[ind] <- d; registers.[4] <- registers.[4] + 1
        | Instr instructions (Inc(Register (REG ind)))-> registers.[ind] <- registers.[ind] + 1; registers.[4] <- registers.[4] + 1
        | Instr instructions (Dec(Register (REG ind)))-> registers.[ind] <- registers.[ind] - 1; registers.[4] <- registers.[4] + 1
        | Instr instructions (Jump(Value d,Value instructionsJump)  )
        | Instr instructions (Jump(Value d,RegVal instructionsJump) )
        | Instr instructions (Jump(RegVal d,RegVal instructionsJump)) 
        | Instr instructions (Jump(RegVal d,Value instructionsJump) )-> if d = 0 then registers.[4] <- registers.[4] + 1
                                                                        else registers.[4] <- registers.[4] + instructionsJump
        | Instr instructions (Toggle(RegVal d)) | Instr instructions (Toggle(Value d)) when registers.[4] + d < instructions.Length -> instructions.[registers.[4] + d] <- toggle instructions.[registers.[4] + d]; registers.[4] <- registers.[4] + 1
        | Instr instructions (Out (RegVal d)  ) | Instr instructions (Out(Value d)   ) -> yield d
        | x -> printfn "Didn't do %A" x; registers.[4] <- registers.[4] + 1
}


let file = "\input.txt"
let instructions = File.ReadAllLines(__SOURCE_DIRECTORY__ + file) |> Array.map parse 
let genSignal = (fun i -> 
                        let signal = doWork [| i;0;0;0;0 |] instructions
                        if signal |> Seq.take 2 |> Seq.chunkBySize 2 |> Seq.take 3 |> Seq.forall (fun [|x;y|] -> x = 0 && y = 1) then (i,true,signal)
                        else (i,false,signal))

Seq.initInfinite genSignal 
|> Seq.skipWhile(fun (x,y,z) -> not y)
|> Seq.take 1
|> Seq.map(fun(n,_,z) -> n,z |> Seq.take 10 |> Seq.toList)
|> Seq.toList

genSignal 198