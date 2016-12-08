#r "bin/Debug/Library.dll"

open Advent.Library

#load "Program.fsx"

open Program

let screen () = Array2D.create 3 7 0
let doTest createScreen commands = 
    let screen = createScreen()
    Array.iter (runCommand screen) commands
    screen
doTest screen [|Rect(3,2)|]
|> is (array2D [[1; 1; 1; 0; 0; 0; 0]
                [1; 1; 1; 0; 0; 0; 0]
                [0; 0; 0; 0; 0; 0; 0]])

doTest screen [|Rect(3,2);RotateColumn(1,1)|]
|> is (array2D [[1; 0; 1; 0; 0; 0; 0]
                [1; 1; 1; 0; 0; 0; 0]
                [0; 1; 0; 0; 0; 0; 0]])

doTest screen [|Rect(3,2);RotateColumn(1,1);RotateRow(0,4)|]
|> is (array2D [[0; 0; 0; 0; 1; 0; 1]
                [1; 1; 1; 0; 0; 0; 0]
                [0; 1; 0; 0; 0; 0; 0]])

doTest screen [|Rect(3,2);RotateColumn(1,1);RotateRow(0,4);RotateColumn(1,1);|]
|> is (array2D [[0; 1; 0; 0; 1; 0; 1]
                [1; 0; 1; 0; 0; 0; 0]
                [0; 1; 0; 0; 0; 0; 0]])