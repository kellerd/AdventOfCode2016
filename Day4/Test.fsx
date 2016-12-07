#r "bin/Debug/Library.dll"

open Advent.Library

#load "Program.fsx"

open Program

let result : Choice<Room list, string> = 
    Choice1Of2([ Real(("aaaaa-bbb-z-y-x".ToCharArray() |> List.ofArray), Sector <| int64 123, Checksum "abxyz") //  is a real room because the most common letters are a (5), b (3), and then a tie between x, y, and z, which are listed alphabetically.
                 Real(("a-b-c-d-e-f-g-h".ToCharArray() |> List.ofArray), Sector <| int64 987, Checksum "abcde") // is a real room because although the letters are all tied (1 of each), the first five are listed alphabetically.
                 Real(("not-a-real-room".ToCharArray() |> List.ofArray), Sector <| int64 404, Checksum "oarel") //is a real room.
                 Decoy(("totally-real-room".ToCharArray() |> List.ofArray), Sector <| int64 200, Checksum "decoy") ]) //is not.

test (extract pallrooms) (input.Trim()) |> is result
test (caesarSolve 26L) [ 'a' ] |> is [ 'a' ]
test (caesarSolve 26L) [ 'z' ] |> is [ 'z' ]
test (caesarSolve (26L * 4L)) [ 'z' ] |> is [ 'z' ]
test (caesarSolve 1L) [ 'z' ] |> is [ 'a' ]
test (caesarSolve 1L) [ 'a' ] |> is [ 'b' ]
test (caesarSolve 343L) [ 'q' ] |> is [ 'v' ]
