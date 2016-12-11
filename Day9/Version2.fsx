#r "bin/Debug/Library.dll"
#r "System.Numerics"

open Advent.Library
open System.Text.RegularExpressions
open System
open System.Numerics

let file = "\input.txt"

type Length = int

type Multiplier = BigInteger

type Node = 
    | Empty
    | Data of char array
    | Marker of Length * Multiplier * Node list

let parse2 (s : string) = 
    let s = Regex.Replace(s, @"\s+", "")
    
    let rec mkData = 
        function 
        | "" -> [ Empty ]
        | s -> 
            match s.IndexOf("(") with
            | -1 -> [ s.ToCharArray() |> Data ]
            | firstBrace -> 
                let (SplitAt firstBrace (left, right)) = s
                (Data left) :: mkMarker right
    
    and mkMarker cs = 
        match String(cs) with
        | "" -> [ Empty ]
        | Prefix "(" (TakeWhile ')' ((Middle "x" (Int(length), "x", Int(multiplier))), rest)) -> 
            let (SplitAt length (left, right)) = rest.Substring(1)
            Marker(length, BigInteger(multiplier), 
                   left
                   |> String
                   |> mkData)
            :: (right
                |> String
                |> mkData)
        | s -> mkData s
    
    mkData s

let rec countLines2 ns = 
    List.sumBy (function 
        | Empty -> BigInteger(0)
        | Data(cs) -> BigInteger(cs.Length)
        | Marker(_, multiplier, ns) -> (countLines2 ns) * multiplier) ns

//    Seq.sumBy (function 
//        | Data(text) -> List.length text
//        | Marker(_, mult, text) -> List.length text * mult)
System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + file)
|> parse2
|> countLines2
