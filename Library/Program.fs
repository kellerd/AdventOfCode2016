namespace Advent

module Library = 
    open System

    let test f textInstructions = f textInstructions
    
    let is expectedResult result = 
        if result <> expectedResult then 
            printfn "Got %A, expected %A" result expectedResult
            false
        else 
            printfn "It worked: %A" result
            true

    let (|Prefix|_|) (p : string) (s : string) = 
        if s.StartsWith(p) then Some(s.Substring(p.Length))
        else None

    let (|Until|_|) (p : char) (s : string) = 
        match s.IndexOf(p) with
        | -1 -> None
        | index -> Some (s.Substring(0,index), s.Substring(index))

    let (|Int|_|) input = 
        input
        |> Int32.TryParse
        |> function 
        | true, int -> Some int
        | _ -> None