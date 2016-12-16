let mkState (str:string) = let arr = str.ToCharArray() 
                           let len = Array.length arr
                           len, (arr |> Seq.ofArray)
                            
let ``0`` = Seq.init 1 (fun _ -> '0')
let dragon (len,state) = 
    let newState = 
        Seq.append ``0`` (Seq.rev state |> Seq.map(function '0' -> '1' | '1' -> '0')) 
        |> Seq.append state 
    len * 2 + 1, newState

let repeatUntil until state  = 
    Seq.unfold (fun (len,str) -> if len > until then None 
                                 else 
                                    let newState = dragon(len,str)
                                    Some (newState,newState)) state

let data max text =
    let data = 
        mkState text
        |> repeatUntil max
        |> Seq.last
        |> snd
        |> Seq.take max
    max,data
let checksum max data= 
    Seq.unfold (fun (len,data) ->
        if len = max || len % 2 = 0 then
            let newData = 
                data
                |> Seq.truncate max
                |> Seq.chunkBySize 2
                |> Seq.rev
                |> Seq.fold (fun chk -> function [|x;y|] when x = y -> '1'::chk | _ -> '0'::chk) []
                |> Seq.rev
            printfn "%A" (Seq.length newData = len / 2)
            let result = ((Seq.length newData),newData)
            (result,result) |> Some
        else None) data
    |> Seq.last 

checksum 12 (12,"110010110100".ToCharArray())
data 20 "10000" |> checksum 20 |> snd |> Seq.toList

let max2 = 35651584

data max2 "00111101111101000"
|> checksum max2  
|> snd 
|> Seq.toArray 
|> System.String


