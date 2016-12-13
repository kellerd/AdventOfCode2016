#r "bin/Debug/Library.dll"
open Advent.Library

#load "Program.fsx"

open Program
floor 10u
test (calculateArea 10u) (4u,7u) |> is {X=7u;Y=4u;State=Open}
test (solve 10u {X=1u;Y=1u;State=Open}) {X=7u;Y=4u;State=Open} |> is 11

test (atMost 10u {X=1u;Y=1u;State=Open}) 1 |> is 3

test (atMost 10u {X=1u;Y=1u;State=Open}) 2 |> is 5