#r "bin/Debug/Library.dll"
open Advent.Library

#load "Program.fsx"
open Program
open System

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

test (parse
      >> getMessage (Seq.minBy snd)
      >> String) (input1.Split([| '\n' |]))
|> is "advent"