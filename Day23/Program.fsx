﻿open System.IO
open System
open System.Numerics
#r "bin/Debug/Library.dll"
open Advent.Library

let (|REG|_|) = function
   | "a" -> Some 0
   | "b" -> Some 1
   | "c" -> Some 2
   | "d" -> Some 3
   | _ -> None
type Value = Register of string | Value of int
type Instr = | Copy of Value * Value | Inc of Value | Dec of Value | Jump of Value * Value | Toggle of Value
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
    | x -> failwith <| sprintf "Couldn't parse %s" x
let toggle  = function
    | Copy (v,v2) -> Jump(v,v2)
    | Inc (v) -> Dec(v) 
    | Dec (v)| Toggle(v) -> Inc(v)
    | Jump (v,i) -> Copy(v,i)
let doWork registers instructions  = 
    let (|RegVal|_|) = function
        | Register (REG ind) -> Some (Array.item ind registers)
        | _ -> None

    while registers.[4] < (Array.length instructions) do
        match registers.[4] |> int |> Array.item <| instructions with
        | Copy(Value d, Register(REG ind)) 
        | Copy(RegVal d, Register(REG ind)) -> registers.[ind] <- d; registers.[4] <- registers.[4] + 1
        | Inc(Register (REG ind)) -> registers.[ind] <- registers.[ind] + 1; registers.[4] <- registers.[4] + 1
        | Dec(Register (REG ind)) -> registers.[ind] <- registers.[ind] - 1; registers.[4] <- registers.[4] + 1
        | Jump(Value d,Value instructionsJump) 
        | Jump(Value d,RegVal instructionsJump) 
        | Jump(RegVal d,RegVal instructionsJump) 
        | Jump(RegVal d,Value instructionsJump) -> if d = 0 then registers.[4] <- registers.[4] + 1
                                                   else registers.[4] <- registers.[4] + instructionsJump
        | Toggle(RegVal d) | Toggle(Value d) when registers.[4] + d < instructions.Length -> instructions.[registers.[4] + d] <- toggle instructions.[registers.[4] + d]; registers.[4] <- registers.[4] + 1
        | x -> printfn "Didn't do %A" x; registers.[4] <- registers.[4] + 1
    registers 


let file = "\input.txt"
File.ReadAllLines(__SOURCE_DIRECTORY__ + file) |> Array.map parse |> doWork [| 7;0;0;0;0 |]
File.ReadAllLines(__SOURCE_DIRECTORY__ + file) |> Array.map parse |> doWork [| 12;0;0;0;0 |]

let input = 
    @"cpy 2 a
tgl a
tgl a
tgl a
cpy 1 a
dec a
dec a".Split([|'\n'|])

input |> Array.map parse |> doWork [| 0;0;0;0;0 |]