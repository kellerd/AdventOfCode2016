#r "bin/Debug/Library.dll"
open Advent.Library

#load "Program.fsx"
open Program
open System.IO


test (breakCode keyPad (2, 2)) input1 |> is (Some "1985")
test (breakCode keyPad2 (3, 1)) input1 |> is (Some "5DB3")
test (breakCode keyPad (2, 2)) (File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")) |> is (Some "99332")