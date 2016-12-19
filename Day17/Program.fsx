#r "bin/Debug/Library.dll"
open System.Security.Cryptography
open Advent.Library


let md5 = MD5.Create()
let startIndex = (0,0)
let input = startIndex,("yjjvjgan" |> System.Text.Encoding.UTF8.GetBytes |> md5.ComputeHash)
let bcdef = "bcdef" |> System.Text.Encoding.UTF8.GetBytes
let DULR = "DULR"  |> System.Text.Encoding.UTF8.GetBytes
let candidate (x,y) (hash:byte[]) = 
    hash.[0..4]
    |> Array.mapi(fun i elem -> if Array.contains elem bcdef then 
                                    let arr = (DULR.[i..i])
                                    let newIndex = 
                                        match i with
                                        | 0 -> x,y+1
                                        | 1 -> x,y-1
                                        | 2 -> x-1,y
                                        | 3 -> x + 1,y
                                    newIndex,(Array.append hash arr)
                                    |> Some
                                else None)

let bfs candidate filter start =
    Seq.unfold (fun (index,current) -> 
                    let nextVals = Seq.choose (candidate index) current
                                    |> Seq.collect filter
                                    |> Seq.toArray
                    if Array.isEmpty nextVals then None
                    else Some (nextVals,nextVals)
                ) [|start|]

let solve = 
    bfs candidate filter (input)
//
