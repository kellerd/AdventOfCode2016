// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
#r "bin/Debug/Library.dll"
#r @"..\packages\FSharp.Collections.ParallelSeq\lib\net40\FSharp.Collections.ParallelSeq.dll"
open Advent.Library
open System.Security.Cryptography
open FSharp.Collections.ParallelSeq

let nibble2 (b:byte) = b &&& 0x0Fuy
let nibble1 (b:byte) = (b &&& 0xF0uy) >>> 4
let md5 = MD5.Create()
let getPassword input = 
    Seq.initInfinite (fun i ->  i,md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes(input + i.ToString())))
    |> PSeq.filter (fun (_,hash) -> 0uy = hash.[0]
                                   && 0uy = hash.[1]
                                   && nibble1 hash.[2] = 0uy)
    |> PSeq.truncate 8
    |> PSeq.map (fun (i,hash) -> i,nibble2 hash.[2])
    |> PSeq.sortBy fst
    |> PSeq.map snd
    |> PSeq.toArray
    |> System.Text.Encoding.UTF8.GetString
    |> (fun x -> x.ToLower())

let getPassword2 input = 
    Seq.initInfinite (fun i ->  md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes(input + i.ToString())))
    |> Seq.filter (fun hash -> 0uy = hash.[0]
                                   && 0uy = hash.[1]
                                   && nibble1 hash.[2] = 0uy
                                   && nibble2 hash.[2] <= 7uy)
    |> Seq.distinctBy (fun h->Array.get h 2 |> nibble2 )
    |> Seq.map (fun h -> Array.get h 3 |> nibble1, (Array.get h 2 |> nibble2))
//    |> Seq.scan (fun map hash -> 
//        let h = nibble2 hash.[2]
//        if Map.containsKey h map then map
//        else Map.add h (nibble1 hash.[3]) map
//    ) Map.empty<byte,byte>
 //   |> Seq.takeWhile(fun map -> map.Count <= 8)  
 //   |> Seq.collect(Map.toSeq)
    |> Seq.toList

test getPassword "abc" |> is "18f47a30"
let input = "cxdnnyjw"
getPassword input
#time
test getPassword2 "abc" |> is "05ace8e3"
getPassword2 input