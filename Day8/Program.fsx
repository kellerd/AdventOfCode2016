open System.IO

#r @"..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsecCS.dll"
#r @"..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsec.dll"

open FParsec

type Commands = 
    | Rect of int * int
    | RotateRow of int * int
    | RotateColumn of int * int

//  rect AxB turns on all of the pixels in a rectangle at the top-left of the screen which is A wide and B tall.
let prect = pstring "rect " >>. pint32 .>> pstring "x" .>>. pint32 |>> Rect
//  rotate row y=A by B shifts all of the pixels in row A (0 is the top row) right by B pixels. Pixels that would fall off the right end appear at the left end of the row.
let protateR = pstring "rotate row y=" >>. pint32 .>> pstring " by " .>>. pint32 |>> RotateRow
//  rotate column x=A by B shifts all of the pixels in column A (0 is the left column) down by B pixels. Pixels that would fall off the bottom appear at the top of the column.
let protateC = pstring "rotate column x=" >>. pint32 .>> pstring " by " .>>. pint32 |>> RotateColumn

let commands : Parser<Commands,unit> = prect <|> protateR <|> protateC

let extract p str = 
    match run p str with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg

let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let rec permutations = function
    | []      -> seq [ [] ]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

let immutableSwap source sourceInd sourceInd2 target targetInd targetInd2 y x =
    let height = target |> Array2D.length1 
    let width = target |> Array2D.length2
    let temp = Array2D.create height width 0
    Array2D.blit target 0 0 temp 0 0 height width
    Array2D.blit source sourceInd sourceInd2 temp targetInd targetInd2 y x
    temp


let rotate rotNum arr = 
    let n = Array.length arr
    [|for k in 0 .. n - 1 ->
         let k = (k + rotNum) % n
         arr.[if k < 0 then n + k else k]|]


let runCommand screen command = 
    match command with 
    | Rect (x,y) -> Array2D.blit (Array2D.create y x 1) 0 0 screen 0 0 y x
    | RotateRow (r, times) -> screen.[r,*] <- rotate -times screen.[r,*]
    | RotateColumn (c,times) -> screen.[*,c] <- rotate -times screen.[*,c]


let width = 50
let height = 6
let screen () = Array2D.zeroCreate height width

let doRun file createScreen commands =
    let screen = createScreen()
    File.ReadAllLines(__SOURCE_DIRECTORY__ + file) |> Array.map (extract commands)
    |> Array.iter (runCommand screen)
    screen

let newScreen = doRun "\input.txt" screen commands

for y in 0 .. 5 do
    printfn "%s" ((newScreen |> Array2D.map(fun num -> if num = 1 then '█' else ' ')).[y,*] |> System.String)