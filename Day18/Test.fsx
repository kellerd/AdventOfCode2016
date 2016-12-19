#r "bin/Debug/Library.dll"
open Advent.Library

#load "Program.fsx"
open Program
open System.IO

//
//test (solve 3 >> Seq.map(Seq.toList) >> Seq.toList) @"..^^.
//"  |> is [[Safe; Safe; Trap; Trap; Safe]
//          [Safe; Trap; Trap; Trap; Trap]
//          [Trap; Trap; Safe; Safe; Trap]]
//
//test (solve 10 >> Seq.map(Seq.toList) >> Seq.toList) ".^^.^.^^^^" 
//|> is  [[Safe;Trap;Trap;Safe;Trap;Safe;Trap;Trap;Trap;Trap;] 
//        [Trap;Trap;Trap;Safe;Safe;Safe;Trap;Safe;Safe;Trap;] 
//        [Trap;Safe;Trap;Trap;Safe;Trap;Safe;Trap;Trap;Safe;] 
//        [Safe;Safe;Trap;Trap;Safe;Safe;Safe;Trap;Trap;Trap;] 
//        [Safe;Trap;Trap;Trap;Trap;Safe;Trap;Trap;Safe;Trap;] 
//        [Trap;Trap;Safe;Safe;Trap;Safe;Trap;Trap;Safe;Safe;] 
//        [Trap;Trap;Trap;Trap;Safe;Safe;Trap;Trap;Trap;Safe;] 
//        [Trap;Safe;Safe;Trap;Trap;Trap;Trap;Safe;Trap;Trap;] 
//        [Safe;Trap;Trap;Trap;Safe;Safe;Trap;Safe;Trap;Trap;] 
//        [Trap;Trap;Safe;Trap;Trap;Trap;Safe;Safe;Trap;Trap;]]

test (countSafe 3) @"..^^.
"  |> is 6
test (countSafe 10) ".^^.^.^^^^"  |> is 38
//test (solve 1 >> Seq.head >> Seq.map rev >> Seq.toArray >> System.String) (System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt"))  |> is (System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt").Trim())
test (solve 3) @"..^^.
" |> is [3;1;2]

test (solve 10 >> Seq.toList)  ".^^.^.^^^^" 
|> is 
 [3;
  5;
  4;
  5;
  3;
  5;
  3;
  3;
  4;
  3;]