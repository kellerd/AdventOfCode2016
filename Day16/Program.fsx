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
    mkState text
    |> repeatUntil max
    |> Seq.last
    |> snd
    |> Seq.take max
let rec checkSum input = 
    let step =
        input
        |> Seq.chunkBySize 2
        |> Seq.map (fun [| x; y |] -> if x = y then '1' else '0')
        |> Seq.toArray
    
    if step.Length % 2 = 0 then 
        step |> Seq.ofArray |> checkSum 
    else step
let max2 = 35651584
data max2 "00111101111101000"
|> checkSum   
|> System.String
