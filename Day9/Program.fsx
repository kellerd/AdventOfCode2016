#r "bin/Debug/Library.dll"

open Advent.Library
open System.Text.RegularExpressions

let file = "\input.txt"

type Text = char list

type Length = int

type Multiplier = int

type Nodes = 
    | Data of Text
    | Marker of Length * Multiplier * Text

let split len mult tail text = function 
    | SplitAt len (captured, rest) -> 
        let m = Marker(len - captured.Length, mult, (captured |> Array.toList) @ text)
        m :: (rest
              |> Array.toList
              |> Data)
             :: tail

let version1 = function 
           | ([] as l) | (Data _ :: _ as l) -> 
               function 
               | Middle "x" (Int(times), "x", Int(multiplier)) -> Marker(times, multiplier, []) :: l
               | text -> 
                   (text.ToCharArray()
                    |> Array.toList
                    |> Data)
                   :: l
           | Marker(0, mult, text) :: _ as l -> 
               function 
               | Middle "x" (Int(times), "x", Int(multiplier)) -> Marker(times, multiplier, []) :: l
               | text -> 
                   (text.ToCharArray()
                    |> Array.toList
                    |> Data)
                   :: l
           | Marker(len, mult, text) :: tail -> 
               function 
               | Middle "x" _ as x -> split len mult tail text ("(" + x + ")")
               | x -> split len mult tail text x

let parse (str : string) = 
    str.Split([| '('; ')' |])
    |> Seq.map (fun str -> Regex.Replace(str, @"\s+", ""))
    |> Seq.filter (Seq.isEmpty >> not)
    |> Seq.fold version1 ([ Data [] ])    
let countLines = 
    Seq.sumBy (function 
        | Data(text) -> List.length text
        | Marker(_, mult, text) -> List.length text * mult)

System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + file)
|> Array.map (parse >> countLines)
|> Array.sum
