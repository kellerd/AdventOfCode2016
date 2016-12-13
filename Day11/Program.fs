open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open System.Collections.Generic

let init = 
    [ [ 2.0 ; 3.0]
      [ 2.0 ; 3.0]
      [ 2.0 ; 3.0]
      [ 2.0 ; 3.0]
      [ 1.0 ; 1.0] ]
    |> matrix

let solved = 
    [ [ 4.0; 4.0 ]
      [ 4.0; 4.0 ]
      [ 4.0; 4.0 ]
      [ 4.0; 4.0 ]
      [ 4.0; 4.0 ] ]
    |> matrix

let operations = 
    [ [ [ 1.0; 1.0 ]
        [ 0.0; 0.0 ]
        [ 0.0; 0.0 ]
        [ 0.0; 0.0 ]
        [ 0.0; 0.0 ] ]
      |> matrix
      [ [ 0.0; 0.0 ]
        [ 1.0; 1.0 ]
        [ 0.0; 0.0 ]
        [ 0.0; 0.0 ]
        [ 0.0; 0.0 ] ]
      |> matrix
      [ [ 0.0; 0.0 ]
        [ 0.0; 0.0 ]
        [ 1.0; 1.0 ]
        [ 0.0; 0.0 ]
        [ 0.0; 0.0 ] ]
      |> matrix
      [ [ 0.0; 0.0 ]
        [ 0.0; 0.0 ]
        [ 0.0; 0.0 ]
        [ 1.0; 1.0 ]
        [ 0.0; 0.0 ] ]
      |> matrix
      [ [ 0.0; 0.0 ]
        [ 0.0; 0.0 ]
        [ 0.0; 0.0 ]
        [ 0.0; 0.0 ]
        [ 1.0; 1.0 ] ]
      |> matrix ]

let zero = 
    [ [ 0.0; 0.0 ]
      [ 0.0; 0.0 ]
      [ 0.0; 0.0 ]
      [ 0.0; 0.0 ]
      [ 0.0; 0.0 ] ]
    |> matrix

let elevator = 1.0

let memoize f = 
    let cache = Dictionary<_, _>()
    fun x ->
        if cache.ContainsKey(x) then None //cache.[x]
        else let res = f x
             cache.[x] <- res
             Some res

let up = 
    [ 1.0,(vector [ 0.0; 1.0 ])
      1.0,(vector [ 1.0; 0.0 ])
      1.0,(vector [ 1.0; 1.0 ]) ]

let down = (up |> List.map (fun (elevator,v) -> elevator * -1.0, v * -1.0))
let both = up @ down
let state = init

let onSameFloor elevator = 
    Matrix.map (fun v -> 
        if v = elevator then 1.0
        else 0.0)

let isMatrixOk m = 
    let nullified = m |> Matrix.reduceCols (fun a b -> Vector.map (abs >> min 1.0) (a - b))
    let nullifiedMatrix = Matrix.mapCols (fun _ col -> col .* nullified) m
    nullifiedMatrix.Column(0)
    |> Vector.existsSkipZeros (fun floor -> nullifiedMatrix.Column(1) |> Vector.existsSkipZeros ((=) floor))
    |> not

let newOps zero operations (elevator,state) = 
    let upOrDown = //Based on where elevator is, can we go up or down
        if elevator = 1. then up
        elif elevator = 4. then down
        else both
    let sameFloor = onSameFloor elevator state
    let len = Seq.length upOrDown
    (List.init len (fun i -> List.map (fun m -> (upOrDown.[i] |> fst) + elevator, Matrix.mapRows (fun _ r -> r .* (upOrDown.[i] |> snd)) (m .* sameFloor)) operations)
         |> List.collect id
         |> List.filter (snd >> (=) zero >> not)
         |> List.distinct
         |> List.map (fun (elevator,v) -> elevator, v + state)
         |> List.filter (fun (_,m) -> isMatrixOk m)
    )

let newOps' zero operations =  memoize (newOps zero operations)

let results zero operations solved = 
    Seq.unfold (fun thisLevelNodes ->
                    let nextNodes = List.choose (newOps' zero operations) thisLevelNodes |> List.collect id |> List.distinctBy (snd)
                    if nextNodes.IsEmpty then None
                    elif nextNodes |> List.exists (snd >> (=) solved) then Some (1,[])
                    else Some (1,nextNodes)
               ) 
[<EntryPoint>]
let main args =
    printfn "%A" ([(elevator,init)] |> results zero operations solved |> Seq.length)
    System.Console.ReadKey() |> ignore
    0


//let init' = 
//    [ [ 2.0; 1.0 ]
//      [ 3.0; 1.0 ] ]
//    |> matrix
//
//let solved' = 
//    [ [ 4.0; 4.0 ]
//      [ 4.0; 4.0 ] ]
//    |> matrix
//
//let operations' = 
//    [ [ [ 1.0; 1.0 ]
//        [ 0.0; 0.0 ] ]
//      |> matrix
//      [ [ 0.0; 0.0 ]
//        [ 1.0; 1.0 ]]
//      |> matrix ]
//
//let zero' = 
//    [ [ 0.0; 0.0 ]
//      [ 0.0; 0.0 ] ]
//    |> matrix
//
//[<EntryPoint>]
//let main args =
//    printfn "%A" ([(elevator,init')] |> results zero' operations' solved' |> Seq.length)
//    System.Console.ReadKey() |> ignore
//    0
