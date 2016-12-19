open System.Collections.Generic
open System.Linq
let elfShuffle1 n =
    let linkedList = System.Collections.Generic.LinkedList(Seq.init n ((+) 1))

    let nextOrLast (current:LinkedListNode<'T> ) =  
        match current.Next with
        | null -> current.List.First
        | x -> x
    let mutable current = linkedList.First
    while linkedList.Count > 1 do
        linkedList.Remove(nextOrLast current)
        current <- nextOrLast current
    current.Value

//elfShuffle1 5
//elfShuffle1 6
//elfShuffle1 3017957

// Make the supplied sequence appear circular but with
// at most 'n' elements, n = -1 elicits infinite repetition.

