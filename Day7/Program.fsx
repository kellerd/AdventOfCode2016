open System.IO
open System

type Hyper = Hyper of char list | Super of char list
type Supports = | TLS | NotTLS | Denied
type Supports2 = | SSL | NotSSL

type ParseState = | PossibleAbba of char list
                  | NotPossible
                  | Abba 
                  | OpenBracket of ParseState * char list

let (|AbbaPos|_|)  = function
    | [ (a,u);(b,x);(c,y);(d,z) ] when (u = z && x = y) && u <> x -> Some a
    | _ -> None
    
let (|AbA|_|)  = function
    | [ u;x;y ] when u = y && u <> x -> Some  [ u;x;y ]
    | _ -> None

let flipAba (Super [a;b;_]) = (Hyper [b;a;b])

let findHyper (str:string) = Seq.fold (fun hyperState elem -> 
                                                match hyperState,elem with
                                                | [] as l, '[' 
                                                | ((Super _::_) as l),'[' -> Hyper [] :: l
                                                | [], elem -> [Super[elem]]
                                                | (Hyper _::_) as l,']' -> Super [] :: l
                                                | ((Super(cs)::tail)), elem -> Super(elem::cs) :: tail
                                                | ((Hyper(cs)::tail)), elem  -> Hyper(elem::cs) :: tail) [] str

let findAbba (str:string) = 
    findHyper str
    |> List.collect (function
                 | Hyper cs -> cs  |> List.mapi  (fun i x -> (i,x)) |> List.windowed 4 |> List.map (function AbbaPos(pos) -> Denied | _ -> NotTLS)
                 | Super cs -> cs |> List.mapi  (fun i x -> (i,x)) |> List.windowed 4  |> List.map (function AbbaPos(pos) -> TLS | _ -> NotTLS))
    |> List.fold (fun currentState elem -> match currentState,elem with | Denied,_ | _,Denied -> Denied | TLS,_ -> TLS | _, x -> x) NotTLS

let findTLS = Array.map (findAbba >>  function | Denied -> NotTLS | x -> x)

let findAba cs = cs |> List.windowed 3 |> List.choose(function AbA x -> Some x | _ -> None)

let findSSL str = 
    let (hypers,supers) = 
        findHyper str 
        |> List.collect (function | Hyper cs -> findAba cs |> List.map Hyper | Super cs -> findAba cs |> List.map Super)
        |> List.partition (function Hyper _ -> true | _ -> false)
    supers |> List.exists (fun super -> List.contains (flipAba super) hypers)

File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt")
|> findTLS
|> Array.countBy id

File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt")
|> Array.filter findSSL
|> Array.length