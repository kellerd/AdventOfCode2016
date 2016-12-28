#r "bin/Debug/Library.dll"
open Advent.Library
open System.Collections.Generic

type Node = {x :int;y:int;size:int;used:int;avail:int;goal:bool}

let nodes data =
    let nodes' = 
        data
        |> Array.skip 1
        |> Array.map 
           (function
             | Match "/dev/grid/node-x(\d+)-y(\d+) +(\d+)T +(\d+)T +(\d+)T +(\d+)%" [Int x;Int y;Int size;Int used;Int avail;Int usepercent]   -> {x = x;y=y;size=size;used=used;avail=avail;goal=false}
            )
    let g = nodes' |> Array.filter (fun n -> n.y = 0) |> Array.maxBy (fun n -> n.x) 
    let gIndex = nodes' |> Array.findIndex ((=) g)
    nodes'.[gIndex] <- {nodes'.[gIndex] with goal=true}
    nodes'
let printNode = 
    (fun (z:Node[,]) -> let s =
                                       [| 
                                       for x in 0..(Array2D.length1 z - 1) do
                                        yield 
                                          [| 
                                            for y in 0..(Array2D.length2 z - 1) do
                                                if z.[x,y].goal then
                                                     yield sprintf "%2d./%2d" z.[x,y].used z.[x,y].size
                                                else
                                                     yield sprintf "%2d /%2d" z.[x,y].used z.[x,y].size
                                           |] |> String.concat "\t"
                                       |] |> String.concat "\n"
                        "\n" + s + "\n")

let nodes2D nodes = 
    nodes
    |> Array.sortBy (fun n -> n.y,n.x)
    |> Array.groupBy (fun n -> n.y)
    |> Array.map(snd)
    |> array2D
let onlyOnce f =
    let cache = System.Collections.Generic.HashSet<_>()
    fun x ->
        if cache.Contains(x) then None
        else 
            cache.Add(x) |> ignore
            Some (f x)
let solve data = 
    //let data = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> Array.skip 1 
    let nodeData = data |> nodes |> nodes2D
    let maxX = Array2D.length2 nodeData - 1
    let maxY = Array2D.length1 nodeData - 1
    let pair A B =
        if A.used <> 0 && A.used <= B.avail && B <> A then
            Some ({A with used=0;avail=A.size;goal=B.goal},  {B with used=B.used+A.used;avail=B.size-B.used-A.used;goal=A.goal})
        else None

    let swap f (nodes2D:Node[,]) (x1,y1) (x2,y2) = 
        if x1 = x2 && y1 = y2 then None
        else 
            f nodes2D.[y1,x1] nodes2D.[y2,x2]
            |> log
            |> Option.map (fun (A,B) -> 
                            let newState = Array2D.copy nodes2D 
                            newState.[y1,x1] <- A
                            newState.[y2,x2] <- B
                            A,newState)
    let adjacent (x,y)  = seq {
        yield (x,y + 1)
        yield (x,y - 1)
        yield (x + 1,y)
        yield (x - 1,y)
    } 
    let flip f x y = f y x
    let nextPlacesEmpty empty = 
        let (e,nodes2D) = empty
        adjacent (e.x,e.y)
        |> Seq.filter(fun (x',y') -> x' >= 0 && y' >= 0 && x' <= maxX && y' <= maxY)
        |> Seq.choose (swap (flip pair) nodes2D (e.x,e.y))
        |> Seq.filter (snd >> (<>) nodes2D)
    let bfs candidate  (start:Node * Node[,])=
        Seq.unfold (fun (current) -> 
                        let nextVals = Seq.choose candidate current
                                       |> Seq.collect id
                                       |> Seq.distinct |> Seq.toArray
                        if Array.isEmpty current then None
                        else Some (current,nextVals)
                    ) [|start|]
    let empty =  [|for x in 0..maxX do
                    for y in 0..maxY do
                        if nodeData.[y,x].size = nodeData.[y,x].avail then
                            yield nodeData.[y,x],nodeData |] |> Seq.head
    bfs (onlyOnce nextPlacesEmpty) empty
    |> Seq.mapi(fun i s -> i,Array.exists (fun (n,_) -> n.x = maxX && n.y = maxY) s)
    //|> Seq.mapi (fun i s -> i,s)
let part1 = 
    let nodes' = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> Array.skip 1 |> nodes
    let byAvail = nodes' |> Array.sortByDescending (fun n -> n.avail)

    let byUsed = nodes' |> Array.sortBy (fun n -> n.used)

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
    combos

let part2 = 
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> Array.skip 1 |> solve 
    |> Seq.length
let part2test = 
    @"Filesystem            Size  Used  Avail  Use%
/dev/grid/node-x0-y0   10T    8T     2T   80%
/dev/grid/node-x0-y1   11T    6T     5T   54%
/dev/grid/node-x0-y2   32T   28T     4T   87%
/dev/grid/node-x1-y0    9T    7T     2T   77%
/dev/grid/node-x1-y1    8T    0T     8T    0%
/dev/grid/node-x1-y2   11T    7T     4T   63%
/dev/grid/node-x2-y0   10T    6T     4T   60%
/dev/grid/node-x2-y1    9T    8T     1T   88%
/dev/grid/node-x2-y2    9T    6T     3T   66%".Split([|'\n'|])
    |> solve
    |> Seq.map (Seq.length)
    |> Seq.length
