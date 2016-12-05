// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
#r "bin/Debug/Library.dll"

open System
open System.IO
open System.Linq
open Advent.Library

type Direction = 
    | North
    | South
    | East
    | West

type Turns = 
    | Left
    | Right

type Distance = 
    | Blocks of int
    static member (+) (Blocks(x), Blocks(y)) = Blocks(x + y)
    static member (-) (Blocks(x), Blocks(y)) = Blocks(x - y)
    static member Abs(Blocks(x)) = abs x |> Blocks

type Move = Turns * Distance

type Address = 
    { North : Distance
      East : Distance
      LastDirection : Direction }

let start = 
    { North = Blocks 0
      East = Blocks 0
      LastDirection = North }

type KeepGoing = 
    | KeepGoing of Move
    | Stop

let parseDir (input : string) = 
    match input.Trim() with
    | Prefix "R" (Int i) -> (Right, Blocks i)
    | Prefix "L" (Int i) -> (Left, Blocks i)
    | x -> failwith <| sprintf "Couldn't match %s" x

let parse (textInstructions : string) = textInstructions.Split([| ',' |]) |> Array.map parseDir
let log = id

//    (fun x -> 
//    printfn "%A" x
//    x)
let move state move = 
    let (newDir, newDistance) = move |> log
    match state.LastDirection, newDir with
    | North, Left | South, Right -> 
        { state with LastDirection = West
                     East = state.East - newDistance }
    | North, Right | South, Left -> 
        { state with LastDirection = East
                     East = state.East + newDistance }
    | East, Right | West, Left -> 
        { state with LastDirection = South
                     North = state.North - newDistance }
    | East, Left | West, Right -> 
        { state with LastDirection = North
                     North = state.North + newDistance }
    |> log

let doMoves = 
    start
    |> log
    |> Array.fold move

let findAddresses input = 
    Seq.append [ start ] <| (Seq.mapFold (fun state m -> 
                                 let newState = move state m
                                 
                                 let result = 
                                     if newState.North = state.North then 
                                         let (Blocks e1, Blocks e2) = newState.East, state.East
                                         
                                         let by = 
                                             if (e1 - e2) > 0 then 1
                                             else -1
                                         seq { 
                                             for i in e2..by..e1 do
                                                 yield { newState with East = Blocks i }
                                         }
                                     else 
                                         let (Blocks e1, Blocks e2) = newState.North, state.North
                                         
                                         let by = 
                                             if (e1 - e2) > 0 then 1
                                             else -1
                                         seq { 
                                             for i in e2..by..e1 do
                                                 yield { newState with North = Blocks i }
                                         }
                                 result |> Seq.skip 1, newState) start input
                             //|> (fun (xs, last) -> xs |> Seq.collect id |> Seq.append [ last ])
                             |> fst
                             |> Seq.collect id)

let sumDistances (x, y) = abs x + abs y
let sumAddress address = sumDistances (address.North, address.East)

let travel textInstructions = 
    parse textInstructions
    |> doMoves
    |> sumAddress

let findFirstDup (addresses : Address seq) = 
    addresses.ToLookup(fun { North = n; East = e } -> (n, e))
    |> Seq.skipWhile (fun t -> t.Count() = 1)
    |> Seq.tryHead

let findTwice textInstructions = 
    (parse textInstructions
     |> findAddresses
     |> findFirstDup)
    |> Option.map (fun g -> g.Key |> (fun (x, y) -> abs x, abs y))
    |> defaultArg
    <| (Blocks -1, Blocks -1)

test travel "R3, L2" |> is (Blocks 5)
test travel "R2, R2, R2" |> is (Blocks 2)
test travel "R5, L5, R5, R3" |> is (Blocks 12)
test travel (File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")) |> is (Blocks 241)
test findTwice "R3, L2" |> is (Blocks -1, Blocks -1)
test findTwice "R2, R2, R2, R2" |> is (Blocks 0, Blocks 0)
test findTwice "R5, L5, R5, R3" |> is (Blocks -1, Blocks -1)
test (findTwice >> sumDistances) "R8, R4, R4, R8" |> is (Blocks 4)
File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
|> travel
|> printfn "%A"
File.ReadAllText(__SOURCE_DIRECTORY__ + "\input2.txt")
|> findTwice
|> sumDistances
|> printfn "%A"
