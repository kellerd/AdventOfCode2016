open System.IO
#r "bin/Debug/Library.dll"
open Advent.Library
#r "../packages/FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"
open FSharp.Collections.ParallelSeq
open System.Collections.Generic

type Airduct = Wall | Space | Interface of int
let mapChars = function
    | '#' -> Wall
    | '.' -> Space
    | CInt x -> Interface x

let onlyOnce2 f =
    let cache = System.Collections.Generic.HashSet<_>()
    fun x y ->
        if cache.Contains(y) then None
        else 
            cache.Add(y) |> ignore
            Some (f x y)

let testData = @"###########
#0.1.....2#
#.#######.#
#4.......3#
###########"

let testData' = testData.Split([|'\n'|]) |> Array.map (fun s -> s.ToCharArray())

let mkData = Array.map (Array.map mapChars) >> array2D

let adjacent (y,x,d,n)  = seq {
    yield (y,x - 1,d + 1,n)
    yield (y,x + 1,d + 1,n)
    yield (y - 1,x,d + 1,n)
    yield (y + 1,x,d + 1,n)
} 

let bfs candidate goal start =
    let (_,_,_,n) = Array.head start
    Seq.unfold (fun (current) -> 
                    let nextVals = 
                            current
                            |> Seq.choose (fun (y,x,d,n) -> candidate d (y,x,n))
                            |> Seq.collect id
                            |> Seq.distinct 
                            |> Seq.toArray
                    if Array.isEmpty current then None
                    elif goal n then Some(current,[||])
                    else Some (current,nextVals)
                ) start

let nextVals (distances:Dictionary<_,_>) nodes d (y,x,n) =
    adjacent (y,x,d,n)
    |> Seq.filter (fun (y,x,d,n) -> 
                    match Array2D.get nodes y x with
                    | Wall -> false
                    | Interface x when n <> x -> 
                        if distances.ContainsKey(x,n) then () 
                        else 
                            distances.Add((n,x),d)
                            distances.Add((x,n),d)
                        true
                    | Interface _ -> false
                    | Space -> true
    ) 
    
let goal (distances:Dictionary<_,_>) (locations:_[]) n =
    let nCount = 
        distances.Keys 
        |> Seq.filter(fun (k1,_) -> k1 = n)
        |> Seq.length
    nCount = (locations.Length - 1)

let rec part1dist finalDest (distances:Dictionary<_,_>) current toCheck distance = 
    if (Set.count toCheck) = 0 then distance + (Option.map (fun finalDest' -> distances.[current,finalDest']) finalDest |> defaultArg <| 0)
    else
        toCheck 
        |> Set.map (fun nextValue ->
                let rest = toCheck.Remove nextValue
                let currentDist = distances.[current,nextValue]
                part1dist finalDest distances nextValue rest (distance + currentDist)
            )
        |> Set.minElement

let collectData findShortestDistance data = 
    let data' = mkData data
    let maxX = (data' |> Array2D.length2) - 1
    let maxY = (data' |> Array2D.length1) - 1
    let locations = 
        [| for x in 0 .. maxX do
            for y in 0 .. maxY do
                match data'.[y,x] with
                | Interface n -> yield (y,x,0,n)
                | _ -> ()
                |]
    let distances = Dictionary<(int*int),int>()
    let toCheck = locations |> Array.map (fun (_,_,_,n) -> n) |> Array.filter ((=) 0 >> not) |> Set.ofArray
    let result = locations |> Seq.collect (Array.singleton >> bfs (onlyOnce2 (nextVals distances data')) (goal distances locations)) |> Seq.toList
    findShortestDistance distances 0 toCheck 0

#time
collectData (part1dist None) testData'


let file = "\input.txt"
File.ReadAllLines(__SOURCE_DIRECTORY__ + file) 
|> Array.map(fun s -> s.ToCharArray())
|> collectData (part1dist None)

File.ReadAllLines(__SOURCE_DIRECTORY__ + file) 
|> Array.map(fun s -> s.ToCharArray())
|> collectData (part1dist (Some 0))