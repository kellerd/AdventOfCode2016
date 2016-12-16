open System.Security.Cryptography
#r "../packages/FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"

open FSharp.Collections.ParallelSeq
open System.Collections.Generic
open System

let hash (input,i) = 
    use md5 = MD5.Create()
    let hashf (input:string) = (input |> System.Text.Encoding.UTF8.GetBytes |> md5.ComputeHash |> BitConverter.ToString).Replace("-","").ToLower()
    let mutable hash' = input + i.ToString()
    for _ in 0..2015 do 
        hash' <- hashf hash' 
    hashf hash'
let input = "yjdafjpo"
let memoize f = 
    let cache = Dictionary<_, _>()
    fun x ->
        if cache.ContainsKey(x) then cache.[x] 
        else let res = f x
             cache.[x] <- res
             res
let hash' = memoize hash
let isEqualSequence n hash = 
    Seq.windowed n hash |> Seq.filter (fun bytes -> bytes |> Seq.forall (fun (b:char) -> b = Seq.head bytes))

let genHashes input = 
    PSeq.init 27824 (fun i -> i,hash' (input,i))
    |> PSeq.filter (fun (i,hash) -> isEqualSequence 3 hash |> Seq.isEmpty |> not)
    |> PSeq.toArray
let hashes = genHashes input
let fives  = 
    let d = System.Collections.Generic.HashSet<_>()
    hashes |> PSeq.iter (fun (i,hash) -> isEqualSequence 5 hash |> PSeq.map(fun bytes -> (i,bytes.[0])) |> PSeq.distinctBy fst |> PSeq.iter (d.Add >> ignore) ) 
    d
let validFive i char = Seq.exists (fun (ind,c) -> ind > i && ind <= i + 1000 && char = c) fives
let solve nth input = 
    hashes
    |> PSeq.filter (fun (i,hash) -> isEqualSequence 3 hash |> Seq.map (Array.item 0) |> Seq.exists(validFive i))
    |> PSeq.sortBy fst
    |> PSeq.toArray
    |> Array.tryItem nth

#time
solve 63 input

