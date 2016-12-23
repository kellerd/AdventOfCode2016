#r "bin/Debug/Library.dll"
open Advent.Library
open System.Collections.Generic

type Node = {x :int;y:int;size:int;used:int;avail:int;goal:bool}

let nodes =
    let nodes' = 
        System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt")
        |> Array.skip 2
        |> Array.map 
           (function
             | Match "/dev/grid/node-x(\d+)-y(\d+) +(\d+)T +(\d+)T +(\d+)T +(\d+)%" [Int x;Int y;Int size;Int used;Int avail;Int usepercent]   -> {x = x;y=y;size=size;used=used;avail=avail;goal=false}
            )
    let g = nodes' |> Array.filter (fun n -> n.y = 0) |> Array.maxBy (fun n -> n.x) 
    let gIndex = nodes' |> Array.findIndex ((=) g)
    nodes'.[gIndex] <- {nodes'.[gIndex] with goal=true}
    nodes'

let byAvail = nodes |> Array.sortByDescending (fun n -> n.avail)

let byUsed = nodes |> Array.sortBy (fun n -> n.used)

let r = (0,0)
let combos = 
    byUsed
    |> Array.filter(fun A -> A.used <> 0)
    |> Array.collect (fun A -> 
                                byAvail 
                                |> Array.takeWhile (fun B -> A.used <= B.avail) 
                                |> Array.filter (fun B -> B <> A)
                                |> Array.map (fun B -> [|A;B|] |> Array.sort) )
    |> Array.distinct
    |> Array.length
let pair A B =
    if A.used <= B.avail && B <> A then
        Some ({A with used=0;avail=A.size;goal=B.goal},  {B with used=B.used+A.used;avail=B.size-B.used-A.used;goal=A.goal})
    else None
let nodes2D = 
    nodes
    |> Array.sortBy (fun n -> n.y,n.x)
    |> Array.groupBy (fun n -> n.x)
    |> Array.map(snd)
    |> array2D

let maxX = Array2D.length1 nodes2D - 1
let maxY = Array2D.length2 nodes2D - 1

let swap (nodes2D:Node[,]) (x1,y1) (x2,y2) = 
    if y2 > maxY || x2 > maxX then None
    else
        pair nodes2D.[x1,y1] nodes2D.[x2,y2]
        |> Option.map (fun (A,B) -> 
                        let newState = Array2D.copy nodes2D
                        newState.[x1,y1] <- A
                        newState.[x2,y2] <- B
                        newState)

let nextPlaces nodes2D (x,y) = 
    if x = 0 && y = 0 then Seq.empty
    else
        
    seq {
        if y > 0u then
            yield (y - 1u,x)
        yield (y + 1u,x)
        if x > 0u then 
            yield (y, x - 1u)
        yield (y, x + 1u)
    }

let bfs candidate filter start =
    Seq.unfold (fun current -> 
                    let nextVals = Seq.choose candidate current
                                    |> Seq.collect filter
                                    |> Seq.toArray
                    if Array.isEmpty nextVals then None
                    else Some (current,nextVals)
                ) [|start|]
