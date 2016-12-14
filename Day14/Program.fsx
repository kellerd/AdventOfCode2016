open System.Security.Cryptography
#r "../packages/FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"
open FSharp.Collections.ParallelSeq
let nibble2 (b : byte) = b &&& 0x0Fuy
let nibble1 (b : byte) = (b &&& 0xF0uy) >>> 4
let md5 = MD5.Create()
let hash input i = md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes(input + i.ToString()))
        

let getPassword nth input = 
    Seq.initInfinite (hash input)
    |> PSeq.mapi (fun id t -> id,t)
    |> PSeq.filter (fun (_,hash) -> Array.windowed 3 hash |> Array.exists (fun [|x;y;z|] -> (x = y) && x = z)
    |> PSeq.sort fst 
    |> Seq.scan (fun (tests,matchesHashes) (index,newHash) ->
                    let foundMatches,newTests = 
                        match tests with 
                        | [] -> [(index,newHash),(index,newHash)]
                        | [first,first'] when first=first' -> [(index,newHash),first]
                        | ((lastIndex,lastHash),_)::tests -> ((index,newHash),(lastIndex,lastHash))::tests |> List.partition (fun ((index2,hash2),(index1,hash1)) -> index2 - index1 < 1000 && Array.windowed 5 hash2 |> Array.exists (fun [|a;b;c;d;e|] -> a = b && b ==c && c == d && d == e)) 
                    let newMatches = foundMatches |> List.map snd

                    newTests,(newMatches @ matchesHashes)
                 ) ([],[])
    |> Seq.takeWhile (fun (_,matchedHashes) -> matchedHashes.Length <= nth)

let getPassword2 input = 
    Seq.initInfinite (hash input)
    |> Seq.filter (fun hash -> 0uy = hash.[0] && 0uy = hash.[1] && hash.[2] <= 7uy)
    |> Seq.distinctBy (fun hash -> hash.[2])
    |> Seq.truncate 8
    |> Seq.sortBy (fun hash -> hash.[2])
    |> Seq.map (fun hash -> nibble1 hash.[3] |> sprintf "%x")
    |> String.concat ""

let input = "yjdafjpo"

getPassword 64 input
getPassword2 input
