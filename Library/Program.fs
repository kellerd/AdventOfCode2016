namespace Advent

module Library = 
    open System

    let test f textInstructions = f textInstructions
    
    let is expectedResult result = 
        if result <> expectedResult then 
            printfn "Got:\r\n%A,\r\nExpected:\r\n%A" result expectedResult
            false
        else 
            printfn "It worked: %A" result
            true

    let (|Prefix|_|) (p : string) (s : string) = 
        if s.StartsWith(p) then Some(s.Substring(p.Length))
        else None

    let (|SplitAt|) (n: int) (s : string) = 
        let n = min n s.Length |> max 0
        s.ToCharArray() |> Array.splitAt n

    
    let (|InTwo|_|) (c: char) (s : char list) = 
        match (s |> Array.ofList |> System.String).Split([| c |]) with
        | [|a;b|] -> Some(a,b)
        | _ -> None
    
    let (|Middle|_|) (p : string) (s : string) = 
        let index = s.IndexOf p
        if index = -1 then None
        else Some(s.Substring(0,index), s.Substring(index,p.Length), (if index + p.Length > s.Length then "" else s.Substring(index + p.Length))) 

    let f = function
        | Middle "x" (a,b,c) -> Some (a,b,c)
        | _ -> None
    
    let (|TakeWhile|_|) (p : char) (s : string) = 
        match s.IndexOf(p) with
        | -1 -> None
        | index -> Some (s.Substring(0,index), s.Substring(index))
    let (|Char|_|) input = 
        input
        |> Char.TryParse
        |> function 
        | true, c -> Some c
        | _ -> None
    let (|Int|_|) input = 
        input
        |> Int32.TryParse
        |> function 
        | true, int -> Some int

    let (|CInt|_|) (input:char) = 
        Convert.ToString(input)
        |> Int32.TryParse
        |> function 
        | true, int -> Some int
        | _ -> None

    let (|Int64|_|) input = 
        input
        |> Int64.TryParse
        |> function 
        | true, int -> Some int
        | _ -> None
    
    open System.Text.RegularExpressions
    open System.Collections.Generic

    let (|Match|_|) (pat:string) (inp:string) =
        let m = Regex.Match(inp, pat) in
        if m.Success
        then Some (List.tail [ for g in m.Groups -> g.Value ])
        else None

    let log x = printfn "%A" x ; x
    
    let logs s x = printfn "%s: %A" s x ; x

    
    let traverseResultM f arr =

        // define the monadic functions
        let (>>=) x f = Option.bind f x
        let retn = Some

        // define a "cons" function
        let cons  = Array.create 1 >> Array.append 

        // right fold over the list
        let initState = retn [||]
        let folder head tail = 
            f head >>= (fun h -> 
            tail >>= (fun t ->
            retn (cons h t) ))

        Array.foldBack folder arr initState 

    let sequenceResultM x = traverseResultM id x

    let memoize f = 
        let cache = Dictionary<_, _>()
        fun x ->
            if cache.ContainsKey(x) then cache.[x] 
            else let res = f x
                 cache.[x] <- res
                 res