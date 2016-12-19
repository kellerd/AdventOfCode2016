#r "bin/Debug/Library.dll"
open Advent.Library

#load "Program.fsx"
open Program
open System.IO


test part1 [|((0,0),"ihgpwlah")|] |> snd |> is "DDRRRD"                        
test part1 [|((0,0),"kglvqrro")|] |> snd |> is "DDUDRLRRUDRD"                  
test part1 [|((0,0),"ulqzkmiv")|] |> snd |> is "DRURDRUDDLLDLUURRDULRLDUUDDDRR"


test part2 [|((0,0),"ihgpwlah")|] |> is 370                     
test part2 [|((0,0),"kglvqrro")|] |> is 492
test part2 [|((0,0),"ulqzkmiv")|] |> is 830