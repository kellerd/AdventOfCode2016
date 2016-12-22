#r "bin/Debug/Library.dll"

open Advent.Library

#load "Program.fsx"
open Program

let testInstructions = 
  [|"swap position 4 with position 0"
    "swap letter d with letter b"
    "reverse positions 0 through 4"
    "rotate left 1 step"
    "move position 1 to position 4"
    "move position 3 to position 0"
    "rotate based on position of letter b"
    "rotate based on position of letter d"
    "rotate right 6 step"|]
let testInstructions' c s = 
  ([|"rotate based on position of letter " + c|], input s)


let testSolve s (sk,t) = 
    (testInstructions |> Array.skip sk |> Array.take t,(input s)) ||> solve |> Seq.toArray |> System.String

test (testSolve "abcdefgh") (0,1) |> is "ebcdafgh"
test (testSolve "abcdefgh") (1,1) |> is "adcbefgh"
test (testSolve "abcdefgh") (2,1) |> is "edcbafgh"
test (testSolve "abcdefgh") (3,1) |> is "bcdefgha"
test (testSolve "abcdefgh") (4,1) |> is "acdebfgh"
test (testSolve "abcdefgh") (5,1) |> is "dabcefgh"
test (testSolve "abcdefgh") (6,1) |> is "ghabcdef"
test (testSolve "abcdefgh") (7,1) |> is "efghabcd"
test (testSolve "abcdefgh") (8,1) |> is "cdefghab"
test (testSolve "abcdefgh") (0,8) |> is "fbdecgha"



testInstructions' "a" "abcdefgh" ||> solve |> Seq.toArray |> System.String  |> Seq.findIndex ((=) 'a')
testInstructions' "b" "abcdefgh" ||> solve |> Seq.toArray |> System.String  |> Seq.findIndex ((=) 'b')
testInstructions' "c" "abcdefgh" ||> solve |> Seq.toArray |> System.String  |> Seq.findIndex ((=) 'c')
testInstructions' "d" "abcdefgh" ||> solve |> Seq.toArray |> System.String  |> Seq.findIndex ((=) 'd')
testInstructions' "e" "abcdefgh" ||> solve |> Seq.toArray |> System.String  |> Seq.findIndex ((=) 'e')
testInstructions' "f" "abcdefgh" ||> solve |> Seq.toArray |> System.String  |> Seq.findIndex ((=) 'f')
testInstructions' "g" "abcdefgh" ||> solve |> Seq.toArray |> System.String  |> Seq.findIndex ((=) 'g')
testInstructions' "h" "abcdefgh" ||> solve |> Seq.toArray |> System.String  |> Seq.findIndex ((=) 'h')

let testReverse s (sk,t) = 
    (testInstructions |> Array.skip sk |> Array.take t |> reverse,(input s)) ||> solve |> Seq.toArray |> System.String

test (testReverse "ebcdafgh") (0,1) |> is   "abcdefgh"
test (testReverse "adcbefgh") (1,1) |> is   "abcdefgh"
test (testReverse "edcbafgh") (2,1) |> is   "abcdefgh"
test (testReverse "bcdefgha") (3,1) |> is   "abcdefgh"
test (testReverse "acdebfgh") (4,1) |> is   "abcdefgh"
test (testReverse "dabcefgh") (5,1) |> is   "abcdefgh"
test (testReverse "ghabcdef") (6,1) |> is   "abcdefgh"
test (testReverse "efghabcd") (7,1) |> is   "abcdefgh"
test (testReverse "cdefghab") (8,1) |> is   "abcdefgh"
test (testReverse "fbdecgha") (0,8) |> is   "abcdefgh"
