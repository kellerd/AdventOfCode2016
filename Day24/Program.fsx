open System.IO
#r "bin/Debug/Library.dll"
open Advent.Library
open System.Collections.Generic

let file = "\input.txt"
File.ReadAllLines(__SOURCE_DIRECTORY__ + file)

type Airduct = Wall | Space | Interface of int
let mapChars = function
    | '#' -> Wall
    | '.' -> Space
    | CInt x -> Interface x

let onlyOnce2 f =
    let cache = System.Collections.Generic.HashSet<_>()
    fun x y ->
        printfn "%A" cache.Count
        if cache.Contains(x) then None
        else 
            cache.Add(x) |> ignore
            Some (f x y)

let testData = @"###########
#0.1.....2#
#.#######.#
#4.......3#
###########"

let testData' = testData.Split([|'\n'|]) |> Array.map (fun s -> s.ToCharArray())

let mkData = Array.map (Array.map mapChars) >> array2D

let data = mkData testData'
let maxX = (data |> Array2D.length2) - 1
let maxY = (data |> Array2D.length1) - 1
let locations = 
    [| for x in 0 .. maxX do
        for y in 0 .. maxY do
            match data.[y,x] with
            | Interface n -> yield (y,x,0,n)
            | _ -> ()
            |]

let distances = Dictionary<(int*int),int>()

let adjacent (y,x,d,n)  = seq {
    yield (y,x - 1,d + 1,n)
    yield (y,x + 1,d + 1,n)
    yield (y - 1,x,d + 1,n)
    yield (y + 1,x,d + 1,n)
} 

let bfs candidate =
    Seq.unfold (fun (current) -> 
                    let nextVals = 
                            Seq.choose candidate current
                                        |> Seq.collect id
                                        |> Seq.distinct |> Seq.toArray
                    if Array.isEmpty current then None
                    elif distances.Count = locations.Length then Some(current,[||])
                    else Some (current,nextVals)
                ) 

let nextVals nodes d (y,x,n) =
    adjacent (y,x,d,n)
    |> Seq.filter (fun (y,x,d,n) -> 
                    match Array2D.get nodes y x with
                    | Wall -> false
                    | Interface x -> 
                        if distances.ContainsKey(x,n) then () 
                        else distances.Add((n,x),d);distances.Add((x,n),d)
                        true
                    | Space -> true
    )
    
//let nextVals' = onlyOnce2 (nextVals data)

//nextVals' 1 (1,1,0) |> Option.map(Seq.toList)