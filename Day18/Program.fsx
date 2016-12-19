#r "bin/Debug/Library.dll"
open Advent.Library
   
let modifyRows len arr = 
    let mutable prevCount = 0
    let mutable left = Array.item 0 arr
    let mutable temp = Array.item 1 arr
    for i in 1..(len-2) do
        prevCount <- prevCount + arr.[i]
        temp <- arr.[i]
        arr.[i] <- 
            (if (left=0 && arr.[i]=0 && arr.[i+1]= 1) || 
               (left=1 && arr.[i]=0 && arr.[i+1]= 0) || 
               (left=0 && arr.[i]=1 && arr.[i+1]= 1) || 
               (left=1 && arr.[i]=1 && arr.[i+1]= 0) then 0
             else 1)
        left <- temp
    prevCount

let mkInput (input:string) = 
    let arr = 
        seq {
            yield 1
            yield! input.Trim().ToCharArray() |> Seq.map(fun c -> if c = '.' then 1 else 0 ) 
            yield 1} 
        |> Seq.toArray
    arr,(arr.Length)
let solve rows input  = 
    let mutable count = 0
    let (input,len) = mkInput input
    for i in 1 .. rows do
        count <- count + modifyRows len input
    count
let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt") 

let part1 () = 
    input
    |> solve 40

let part2 () = 
    input
    |> solve 400000

part2()

#r "bin/Debug/Library.dll"
open Advent.Library



let nextRow r = 
    seq {
        yield  1
        yield! r
        yield  1
    } |> Seq.windowed 3
      |> Seq.map newTile
let solve rows input  = 
    let mutable total = 0
    let mutable row = input
    for i in 0..(rows-1) do
        total <- total + Seq.sum row
        row <- nextRow row
        if i % 500 = 0 then printfn "%d" i
    total
let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")  |> mkInput    
let part1 () = 
    input
    |> solve 40
let part2 () = 
    input
    |> solve 400000

#time
part2()
