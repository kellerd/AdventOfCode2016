// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
#r "bin/Debug/Library.dll"

open Advent.Library
open System.IO
open System

let parse input = 
    input
    |> Array.filter (fun (str : string) -> str.Length > 0)
    |> Array.map (fun (str : string) -> str.Trim().ToCharArray())
    |> array2D

let getMessage chooserAlgorithm arr : char [] = 
    let letterLen = Array2D.length2 arr
    //.[*,column] gets row as array, .[*,column..column2] gets as matrix
    Array.init letterLen (fun column -> 
        arr.[*, column]
        |> Seq.cast<'a>
        |> Seq.countBy id
        |> chooserAlgorithm
        |> fst)

let input1 = "eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar
"

test (parse
      >> getMessage (Seq.maxBy snd)
      >> String) (input1.Split([| '\n' |]))
|> is "easter"
File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt")
|> parse
|> getMessage (Seq.maxBy snd)
|> String
test (parse
      >> getMessage (Seq.minBy snd)
      >> String) (input1.Split([| '\n' |]))
|> is "advent"
File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt")
|> parse
|> getMessage (Seq.minBy snd)
|> String
