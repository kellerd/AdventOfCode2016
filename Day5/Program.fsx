// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
#r "bin/Debug/Library.dll"

open Advent.Library
open System.IO
open System
open System.Security.Cryptography
let md5 = MD5.Create()
let x = [1..100000] |> List.map (string >> System.Text.Encoding.ASCII.GetBytes)
List.map (fun (buffer:byte[]) -> md5.ComputeHash(buffer)) x
