#r "bin/Debug/Library.dll"
open Advent.Library

#load "Program.fsx"
open Program
open System.IO


test getPassword "abc" |> is "18f47a30"
test getPassword2 "abc" |> is "05ace8e3"