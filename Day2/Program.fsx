open System.IO
open System
#r "bin/Debug/Library.dll"
open Advent.Library
let keyPad = 
    array2D [ [ None; None; None; None; None ]
              [ None; Some '1'; Some '2'; Some '3'; None ]
              [ None; Some '4'; Some '5'; Some '6'; None ]
              [ None; Some '7'; Some '8'; Some '9'; None ]
              [ None; None; None; None; None ] ]

let keyPad2 = 
    array2D [ [ None; None; None; None; None; None; None ]
              [ None; None; None; Some '1'; None; None; None ]
              [ None; None; Some '2'; Some '3'; Some '4'; None; None ]
              [ None; Some '5'; Some '6'; Some '7'; Some '8';Some  '9'; None ]
              [ None; None; Some 'A'; Some 'B'; Some 'C'; None; None ]
              [ None; None; None; Some 'D'; None; None; None ]
              [ None; None; None; None; None; None; None ] ]

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
    | None -> y, x
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
    |> sequenceResultM
    |> Option.map String

File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
|> breakCode keyPad (2, 2)
|> printfn "%A"
File.ReadAllText(__SOURCE_DIRECTORY__ + "\input2.txt")
|> breakCode keyPad2 (3, 1)
|> printfn "%A"
