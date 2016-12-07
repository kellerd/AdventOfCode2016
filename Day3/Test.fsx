#r "bin/Debug/Library.dll"
open Advent.Library

#load "Program.fsx"
open Program
let input3 = "
101 301 501
102 302 502
103 303 503
201 401 601
202 402 602
203 403 603
"

test findTriangles input1 |> is [||]
test findTriangles input2 |> is (Array.replicate 6 ([| 3; 4; 5 |]))
test countTriangles input1 |> is 0
test countTriangles input2 |> is 6
test findTriangles2 input3 |> is
test countTriangles2 input3 |> is 6