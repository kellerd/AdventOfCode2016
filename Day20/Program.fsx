#r "bin/Debug/Library.dll"
open Advent.Library

let file = "\input.txt"
type IPRange = Range of uint32 * uint32
let toRange arr = (Array.item 0 arr |> uint32, Array.item 1 arr |> uint32) |> Range
let parse = Array.map(fun (s:string) -> s.Split([|'-'|]) |> toRange)

let collapse ranges = 
    ranges 
    |> Array.sortBy (fun (Range(a,b)) -> a)
    |> Array.fold (fun state elem ->
                  match state,elem with
                  | [],Range(loB,hiB) -> [Range(loB,hiB)]
                  | Range(_,hiA )::_,Range(_,hiB) when hiA >= hiB -> state
                  | Range(loA,hiA )::tail,Range(loB,hiB) when hiA + 1u >= loB -> Range (loA,hiB)::tail
                  | _,_ -> elem::state) []
    |> List.rev

#time
let invalid = 
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + file)
    |> parse 
    |> collapse 

let valid = Seq.unfold (fun (elem,states) ->
                            match elem,states with
                            | _,[] -> None
                            | next,Range(loB,hiB)::_ when next >= loB && hiB = System.UInt32.MaxValue -> None
                            | next,Range(loB,hiB)::tail when next >= loB -> Some([||],(hiB + 1u,tail))
                            | next,Range(loB,hiB)::_    when next < loB && hiB = System.UInt32.MaxValue -> Some([|next..loB-1u|],(hiB,[]))
                            | next,Range(loB,hiB)::tail when next < loB -> Some([|next..loB-1u|],(hiB + 1u,tail))
                       ) (0u,invalid) 
            |> Seq.filter ((=) [||] >> not)
            |> Seq.collect id

let part1 = valid |> Seq.head
let part2 = valid |> Seq.length
