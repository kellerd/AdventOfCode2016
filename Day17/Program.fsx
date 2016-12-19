#r "bin/Debug/Library.dll"
open System.Security.Cryptography
open Advent.Library
let nibble2 (b : byte) = b &&& 0x0Fuy
let nibble1 (b : byte) = (b &&& 0xF0uy) >>> 4
let md5 = MD5.Create()
let startIndex = (0,0)
let compute (input:string) = input |> System.Text.Encoding.UTF8.GetBytes |> md5.ComputeHash 
let input = [|startIndex,"yjjvjgan"|]
let bcdef =[|  (0xbuy) ; (0xcuy);(0xduy);(0xeuy);(0xfuy);|]
let UDLR = "UDLR"
let candidate ((x,y),(input:string)) = 
    if (x = 3 && y = 3) then
        [|None|]
    else
        (input |> compute).[0..1] 
        |> Array.collect (fun b -> [|nibble1 b;nibble2 b|])
        |> Array.mapi(fun i elem ->     
            let (newX,newY) = 
                match i with
                | 0 -> x,y-1
                | 1 -> x,y+1
                | 2 -> x-1,y
                | 3 -> x + 1,y
    
            if Array.contains elem bcdef && newX >= 0 && newX < 4 && newY >= 0 && newY < 4 then 
                Some((newX,newY),(input + UDLR.[i..i]))
            else None)

let bfs candidate (start) =
    Seq.unfold (fun (arr) -> 
                    let nextVals = Array.collect candidate arr |> Array.choose id
                    let ends = nextVals |> Array.filter (fun (index,_) -> index = (3,3))
                    if Array.isEmpty ends |> not then Some (ends,nextVals)
                    elif Array.isEmpty nextVals then None
                    else Some ([||],nextVals)
                ) start

let solve input = 
    let text = input |> Array.head |> snd
    bfs candidate input 
    |> Seq.collect (fun arr -> arr
                               |> Array.map (fun (ind,s:string) -> ind,s.Replace(text,"")))
let part1 input = solve input |> Seq.head
let part2 input = solve input |> Seq.sortByDescending (snd >> String.length) |> Seq.take 1 |> Seq.head |> snd |> String.length 


part2 [|(0,0),"yjjvjgan"|]