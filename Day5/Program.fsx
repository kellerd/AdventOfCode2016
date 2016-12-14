open System.Security.Cryptography

let nibble2 (b : byte) = b &&& 0x0Fuy
let nibble1 (b : byte) = (b &&& 0xF0uy) >>> 4
let md5 = MD5.Create()
let hash input i = md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes(input + i.ToString()))

let getPassword nth input = 
    Seq.initInfinite (hash input)
    |> Seq.filter (fun hash -> 0uy = hash.[0] && 0uy = hash.[1] && nibble1 hash.[2] = 0uy)
    |> Seq.truncate 8
    |> Seq.map (fun hash -> nibble2 hash.[2] |> sprintf "%x")
    |> String.concat ""

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
