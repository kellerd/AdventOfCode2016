open System.IO
open System

#r @"..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsecCS.dll"
#r @"..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsec.dll"

open FParsec
open System.Linq

type CheckSum = 
    | Checksum of string

type Sector = 
    | Sector of int64

type Room = 
    | Real of char list * Sector * CheckSum
    | Decoy of char list * Sector * CheckSum

let extract p str = 
    match run p str with
    | Success(result, _, _) -> Choice1Of2 result
    | Failure(errorMsg, _, _) -> Choice2Of2 errorMsg

let checkChecksum cl sec cks = 
    //let cl = Seq.concat cll
    let letters = 
        let top5 = 
            (cl
             |> Seq.countBy id
             |> Seq.filter (fun (x, y) -> x <> '-')).OrderByDescending(fun (x, y) -> y).ThenBy(fun (x, y) -> x).Take 5
            |> Seq.toArray
            |> Array.unzip
            |> fst
        Checksum(String(top5))
    if letters = cks then Real(cl |> Seq.toList, sec, cks)
    else Decoy(cl |> Seq.toList, sec, cks)

let pChecksum = manyMinMaxSatisfy 5 5 isLower
                |> between (pstring "[") (pstring "]")
                |>> Checksum
// |>> Checksum
let pdash = pchar '-'
let pSector = pdash >>. pint64 |>> Sector
let plist = many (notFollowedBy pSector >>. (lower <|> pdash))
let proom = pipe3 plist pSector pChecksum checkChecksum
let pallrooms : Parser<Room list, unit> = sepBy1 proom newline
let input = "
aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]
"

let collectSecorIds = 
    function 
    | Choice1Of2 rooms -> 
        rooms |> List.map (function 
                     | Real(_, Sector(i), _) -> Numerics.BigInteger(i)
                     | Decoy(_) -> Numerics.BigInteger(0))
    | Choice2Of2 x -> failwith <| sprintf "%A couldn't parse" x

File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt").Trim()
|> extract pallrooms
|> collectSecorIds
|> List.sum

let caesarSolve num room = 
    let rotation = num % 26L
    let asc (c : char) = Convert.ToInt64(c) - 96L
    List.map (fun c -> 
        if c = '-' then ' '
        else (asc 'a') + (((asc c) - (asc 'a')) + rotation) % 26L + 96L |> Convert.ToChar) room

let mapCaesar = 
    pallrooms |>> (List.filter (function 
                       | Real(room, Sector num, _) -> true
                       | _ -> false)
                   >> List.map (fun (Real(room, Sector num, _)) -> 
                          num,caesarSolve num room
                          |> Array.ofList
                          |> String)
                   >> List.filter (fun (_,str) -> str.Contains("north")))

File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt").Trim() |> extract mapCaesar
