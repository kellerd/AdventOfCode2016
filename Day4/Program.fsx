// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
#r "bin/Debug/Library.dll"
open Advent.Library
open System.IO
open System
#r @"C:\Users\diese\Documents\Visual Studio 2015\Projects\AdventOfCode\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsec.dll"
#r @"C:\Users\diese\Documents\Visual Studio 2015\Projects\AdventOfCode\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsecCS.dll"

open FParsec
open System.Linq
type CheckSum = Checksum of string
type Sector = Sector of int64
type Room = 
   | Real of char list * Sector * CheckSum
   | Decoy  of char list * Sector * CheckSum

let checkCheckSum ( cl,sec,cks)= 
  let letters = (Seq.countBy id cl).OrderByDescending(fun (x,y) -> y).ThenBy(fun (x,y) -> x).Take 5 |> dtring
  if letters = cks then
      Real 
  else
      Decoy

let str s = pstring s
let pSector = pint64 |>> Sector
let pChecksum = str "[" >>. manyMinMaxSatisfy 5 5 isLower .>> str "]" |>> Checksum
let dash = str "-"
let parseList = sepBy lower dash 
let room = 
   parseList .>>. pSector .>>. pChecksum 
   |>> checkChecksum


let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
|> parse
|> collectSecorIds
|> sum
|> printfn "%s"