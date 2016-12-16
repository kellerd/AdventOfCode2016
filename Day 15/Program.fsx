#r "bin/Debug/Library.dll"

open Advent.Library
type Disc = {Num:int;Positions:int;Offset:int}
let collectDisc = function | Match "Disc #(\d+) has (\d+) positions; at time=(\d+), it is at position (\d+)." [num;positions;time;offBy] -> {Num=int num;Positions=int positions;Offset=int offBy-int time} 
let file = "\input.txt"

let solve' (discs : Disc list) = 
  // what's the first time we can press and get through the first slot?
  let fstDisc    = discs.[0]
  let timeToZero = fstDisc.Positions - fstDisc.Offset
  let fstT       = timeToZero - fstDisc.Num

  Seq.initInfinite (fun i -> i * fstDisc.Positions + fstT)
  |> Seq.filter (fun t ->
    discs |> List.forall (fun disc ->
      (t + disc.Num - (disc.Positions - disc.Offset)) % disc.Positions = 0))
  |> Seq.head


let solve file = 
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + file) 
    |> Array.map (collectDisc)  
//    |> Array.filter(fun {Num=num} -> num = 4 || num = 3)
    |> Array.toList
    |> List.append [{Num=7;Positions=11;Offset=0}]
    |> solve'


solve @"\input.txt" //317371