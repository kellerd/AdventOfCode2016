#r "bin/Debug/Library.dll"
open Advent.Library

#load "Program.fsx"
open Program
open System.IO

test travel "R3, L2" |> is (Blocks 5)
test travel "R2, R2, R2" |> is (Blocks 2)
test travel "R5, L5, R5, R3" |> is (Blocks 12)
test travel (File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")) |> is (Blocks 241)
test findTwice "R3, L2" |> is (Blocks -1, Blocks -1)
test findTwice "R2, R2, R2, R2" |> is (Blocks 0, Blocks 0)
test findTwice "R5, L5, R5, R3" |> is (Blocks -1, Blocks -1)
test (findTwice >> sumDistances) "R8, R4, R4, R8" |> is (Blocks 4)
