#r "bin/Debug/Library.dll"

open Advent.Library

#load "Program.fsx"
#load "Version2.fsx"

open Program
open Version2


test (parse >> countLines) "ADVENT" |> is 6
test (parse >> countLines) "A(1x5)BC" |> is 7
test (parse >> countLines) "(3x3)XY   Z" |> is 9
test (parse >> countLines) "A(2   x2)BCD(   2x2)EFG" |> is 11
test (parse >> countLines) "(6x  1)(1x3)A" |> is 6
test (parse >> countLines) "X(8x2)(3x3)ABCY" |> is 18

test (parse2 >> countLines2) "ADVENT" |> is 6
test (parse2 >> countLines2) "A(1x5)BC" |> is 7
test (parse2 >> countLines2) "(3x3)XYZ" |> is 9
test (parse2 >> countLines2) "A(2   x2)BCD(   2x2)EFG" |> is 11
test (parse2 >> countLines2) "(6x  1)(1x3)A" |> is 3
test (parse2 >> countLines2) "(27x12)(20x12)(13x14)(7x10)(1x12)A" |> is 241920
test (parse2 >> countLines2) "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" |> is 445