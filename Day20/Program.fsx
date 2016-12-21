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
                  | [],Range(rbl,rbh) -> [Range(rbl,rbh)]
                  | Range(ral,rah )::tail,Range(rbl,rbh) when rah >= rbh -> state
                  | Range(ral,rah )::tail,Range(rbl,rbh) when rah + 1u >= rbl -> Range (ral,rbh)::tail
                  | tail,Range(rbl,rbh) -> elem::tail) []
    |> List.rev

#time
let invalid = 
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + file)
    |> parse 
    |> Array.sortBy (fun (Range(a,b)) -> a)
    |> collapse 

let valid = Seq.unfold (fun (elem,states) ->
                            match elem,states with
                            | _,[] -> None
                            | next,Range(rbl,rbh)::_ when next >= rbl && rbh = System.UInt32.MaxValue -> None
                            | next,Range(rbl,rbh)::tail when next >= rbl -> Some([||],(rbh + 1u,tail))
                            | next,Range(rbl,rbh)::_    when next < rbl && rbh = System.UInt32.MaxValue -> Some([|next..rbl-1u|],(rbh,[]))
                            | next,Range(rbl,rbh)::tail when next < rbl -> Some([|next..rbl-1u|],(rbh + 1u,tail))
                       ) (0u,invalid) 
            |> Seq.filter ((=) [||] >> not)
            |> Seq.collect id

let part1 = valid |> Seq.head
let part2 = valid |> Seq.length
