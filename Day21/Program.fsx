#r "bin/Debug/Library.dll"
open Advent.Library
open System.Collections.Generic

let input s = 
    let dict = Dictionary<char,int>()
    s |> Seq.iteri (fun i c -> dict.Add (c,i))
    dict
let solve instructions  (input:Dictionary<char,int>) =
    let len = input.Count 
    let kvp = Seq.map(fun (kv:KeyValuePair<_,_>) -> kv.Key, kv.Value) >> Seq.toArray
    let rot n f =
        let n' = n % len
        [0..n'-1] |> Seq.iter (fun _ -> f (kvp input))
    let rotL = Seq.iter(fun kv -> match snd kv with | 0 -> input.Item(fst kv) <- len - 1 | v' -> input.Item(fst kv) <- v' - 1 ) 
    let rotR = Seq.iter(fun kv -> match snd kv with | v' when v' = len - 1 -> input.Item(fst kv) <- 0 | v' -> input.Item(fst kv) <- v' + 1 ) 
    let rotP c = 
        match input.Item(c) with
        | x when x >= 4 -> rot (x+2) rotR 
        | x -> rot (x+1) rotR 
    let rotPL c = 
        match input.Item(c) with
        | 0 -> rot 1 rotL  
        | 1 -> rot 1 rotL  
        | 2 -> rot 6 rotL  
        | 3 -> rot 2 rotL   
        | 5 -> rot 3 rotL   
        | 4 -> rot 7 rotL   
        | 6 -> rot 8 rotL   
        | 7 -> rot 4 rotL
    let swap k k2 =
        let temp = input.Item(k)
        input.Item(k) <- input.Item(k2)
        input.Item(k2) <- temp
    let swapP n m = 
        let k = input |> Seq.find (fun kv -> kv.Value = n)
        let k2 = input |> Seq.find(fun kv -> kv.Value = m)
        swap k.Key k2.Key
    let swapv vkey vmapTo =
        let map = Map.ofList (List.zip vkey vmapTo)
        kvp input |> Array.iter(fun kv -> Map.tryFind (snd kv) map |> Option.iter (fun v -> input.Item(fst kv) <- v))
    let reverse n m = 
        swapv [n .. m] [m .. -1 .. n]
    let move n m =
        match n,m with
        | n,m when n > m -> swapv (n::[m..n-1]) [m..n]
        | n,m when n <= m -> swapv  [n..m] (m::[n..m-1])

    instructions 
    |> Array.iter(function
                    | Match "rotate left (\d+) step" [Int n]   -> rot n rotL
                    | Match "rotate right (\d+) step" [Int n]   -> rot n rotR 
                    | Match "swap position (\d+) with position (\d+)" [Int n;Int m]   -> swapP n m
                    | Match "swap letter ([A-Za-z]) with letter ([A-Za-z])" [Char a;Char b]  -> swap a b
                    | Match "reverse positions (\d+) through (\d+)" [Int n;Int m]   -> reverse n m 
                    | Match "move position (\d+) to position (\d+)" [Int n;Int m]   -> move n m 
                    | Match "rotate based on position of letter ([A-Za-z])" [Char a]   -> rotP a
                    | Match "rotate left based on position of letter ([A-Za-z])" [Char a]   -> rotPL a
                    | x -> failwith <| sprintf "Couldn't match %s" x)

    input |> Seq.sortBy (fun kv -> kv.Value) |> Seq.map (fun kv -> kv.Key)




(System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt"),(input "abcdefgh"))
||> solve |> Seq.toArray |> System.String

let reverse instructions = 
    instructions 
    |> Array.map (function
                    | Match "rotate left (\d+) step" [Int n]   -> sprintf "rotate right %d step" n
                    | Match "rotate right (\d+) step" [Int n]   -> sprintf "rotate left %d step" n
                    | Match "swap position (\d+) with position (\d+)" _  as s -> s
                    | Match "swap letter ([A-Za-z]) with letter ([A-Za-z])" _ as s -> s
                    | Match "reverse positions (\d+) through (\d+)" _ as s  -> s
                    | Match "move position (\d+) to position (\d+)" [Int n;Int m]   -> sprintf "move position %d to position %d" m n
                    | Match "rotate based on position of letter ([A-Za-z])" [Char a]  -> sprintf "rotate left based on position of letter %c" a
                    | x -> failwith <| sprintf "Couldn't match %s" x) 
    |> Array.rev


(System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> reverse,(input "fbgdceah"))
||> solve |> Seq.toArray |> System.String