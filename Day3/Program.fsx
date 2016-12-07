// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
#r "bin/Debug/Library.dll"

open Advent.Library
open System.IO
open System

let parse (textInstructions : string) = 
    textInstructions.Trim().Split([| '\n' |]) |> Array.map (fun str -> 
                                                     str.Split([| ' ' |])
                                                     |> Array.map (fun str -> str.Trim())
                                                     |> Array.filter (fun str -> str.Length > 0))

let testTriangle = 
    function 
    | [| Int a; Int b; Int c |] -> 
        [| a; b; c |]
        |> Array.sort
        |> function 
        | [| a; b; c |] when a + b > c -> Some [| a; b; c |]
        | _ -> None
    | _ -> None

let findTriangles = parse >> Array.choose testTriangle
let countTriangles = findTriangles >> Array.length
let input1 = "     5 10 25
10 5          25
5 25 10
25         10 5
25 5 10
10         25 5
"
let input2 = "
3 4 5
3 5 4
4 3 5
4 5 3
5 3 4
5 4 3
"

let parse2 (textInstructions : string) = 
    textInstructions.Split([| ' '; '\r'; '\n' |])
    |> Array.map (fun str -> str.Trim())
    |> Array.filter (fun str -> str.Length > 0)
    |> Array.chunkBySize 9
    |> Array.collect (Array.windowed 7)

let findTriangles2 = 
    parse2 >> Array.choose (fun xs -> 
                  testTriangle [| xs.[0]
                                  xs.[3]
                                  xs.[6] |])

let countTriangles2 = findTriangles2 >> Array.length

File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
|> countTriangles
|> printfn "%A"
File.ReadAllText(__SOURCE_DIRECTORY__ + "\input2.txt")
|> countTriangles2
|> printfn "%A"
