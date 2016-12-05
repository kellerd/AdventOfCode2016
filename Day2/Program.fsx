// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
#r "bin/Debug/Library.dll"

open Advent.Library
open System.IO
open System

let keyPad = 
    array2D [ [ 'x'; 'x'; 'x'; 'x'; 'x' ]
              [ 'x'; '1'; '2'; '3'; 'x' ]
              [ 'x'; '4'; '5'; '6'; 'x' ]
              [ 'x'; '7'; '8'; '9'; 'x' ]
              [ 'x'; 'x'; 'x'; 'x'; 'x' ] ]

let keyPad2 = 
    array2D [ [ 'x'; 'x'; 'x'; 'x'; 'x'; 'x'; 'x' ]
              [ 'x'; 'x'; 'x'; '1'; 'x'; 'x'; 'x' ]
              [ 'x'; 'x'; '2'; '3'; '4'; 'x'; 'x' ]
              [ 'x'; '5'; '6'; '7'; '8'; '9'; 'x' ]
              [ 'x'; 'x'; 'A'; 'B'; 'C'; 'x'; 'x' ]
              [ 'x'; 'x'; 'x'; 'D'; 'x'; 'x'; 'x' ]
              [ 'x'; 'x'; 'x'; 'x'; 'x'; 'x'; 'x' ] ]

type Instructions = 
    | Left
    | Up
    | Right
    | Down

let mapInstr = 
    function 
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | 'R' -> Right
    | c -> failwith <| sprintf "Found invalid char %c" c

let input1 = @"ULL
RRDDD
LURDL
UUUUD
"
let parse (textInstructions : string) = textInstructions.Trim().Split([| '\n' |])

let nextKey keyPad (y, x) instruction = 
    let newResult = 
        match instruction with
        | Up -> y - 1, x
        | Down -> y + 1, x
        | Left -> y, x - 1
        | Right -> y, x + 1
    match newResult ||> Array2D.get keyPad with
    | 'x' -> y, x
    | _ -> newResult

let foldCodes keyPad start codes = 
    let state = Array.fold (nextKey keyPad) start codes
    (state ||> Array2D.get keyPad, state)

let breakCode keyPad start input = 
    input
    |> parse
    |> Array.mapFold (fun state str -> 
           str.ToCharArray()
           |> Array.map mapInstr
           |> foldCodes keyPad state) start
    |> fst
    |> String

test (breakCode keyPad (2, 2)) input1 |> is "1985"
test (breakCode keyPad2 (3, 1)) input1 |> is "5DB3"
test (breakCode keyPad (2, 2)) (File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")) |> is "99332"
File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
|> breakCode keyPad (2, 2)
|> printfn "%s"
File.ReadAllText(__SOURCE_DIRECTORY__ + "\input2.txt")
|> breakCode keyPad2 (3, 1)
|> printfn "%s"
