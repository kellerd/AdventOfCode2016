type State = | Open | Wall
type Location = {X:uint32;Y:uint32;State:State}

let start = {X=1u;Y=1u;State=Open}

let calculateArea (favNumber:uint32) (y,x) = 
    Seq.unfold (fun (isSpace,num) -> match num with
                                     | 0u -> None
                                     | n -> Some (not isSpace,(not isSpace,n &&& (n - 1u)))) (true,(x*x + 3u*x + 2u*x*y + y + y*y + favNumber))
    |> Seq.last
    |> function 
    | true -> {X=x;Y=y;State=Open}
    | _ -> {X=x;Y=y;State=Wall}
let map = function
    | {State=Wall} -> "#"
    | {State=Open} -> "."

let floor favNumber = 
    [for y in 0u .. 9u do
       yield 
        [for x in 0u.. 9u do
            yield map(calculateArea favNumber (y,x))]
    ]

let onlyOnce f =
    let cache = System.Collections.Generic.HashSet<_>()
    (cache, fun x ->
                if cache.Contains(x) then None
                else 
                    cache.Add(x) |> ignore
                    Some (f x))
    
let nextPlaces {X=x;Y=y;} = 
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

let solve favNumber start target = 
    let nextPlaces' = onlyOnce nextPlaces |> snd
    let calculateArea' = onlyOnce (calculateArea favNumber) |>snd
    let filter vals = vals |> Seq.choose calculateArea' |> Seq.filter(fun {State=s} -> s = Open)
    bfs nextPlaces' filter start |> Seq.takeWhile (fun vals -> vals |> Array.contains (target) |> not) |> Seq.length

let atMost favNumber start moves =  
    let take = moves + 1
    let (placesCache,nextPlaces') = onlyOnce nextPlaces 
    let calculateArea' = onlyOnce (calculateArea favNumber) |>snd
    let filter vals = vals |> Seq.choose calculateArea' |> Seq.filter(fun {State=s} -> s = Open)
    bfs nextPlaces' filter start |> Seq.truncate take |> Seq.toArray |> ignore 
    placesCache.Count


solve 1350u {X=1u;Y=1u;State=Open} {X=31u;Y=39u;State=Open}
atMost 1350u {X=1u;Y=1u;State=Open} 50