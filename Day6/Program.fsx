open System.IO
open System

let parse input = 
    input
    |> Array.map (fun (str : string) -> str.ToCharArray())
    |> array2D

let genColumn (arr:char[,]) chooserAlgorithm column =
    arr.[*, column]
    |> Seq.countBy id
    |> chooserAlgorithm
    |> fst

let getMessage chooserAlgorithm arr = 
    Array.init 
        (Array2D.length2 arr) 
        (genColumn arr chooserAlgorithm )

File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt")
|> parse
|> getMessage (Seq.maxBy snd)
|> String

File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt")
|> parse
|> getMessage (Seq.minBy snd)
|> String
