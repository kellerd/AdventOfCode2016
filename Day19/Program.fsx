open System.Collections.Generic
open System.Linq

let nextOrLast (current:LinkedListNode<'T> ) =  
        match current.Next with
        | null -> current.List.First
        | x -> x

let elfShuffle1 n =
    let linkedList = System.Collections.Generic.LinkedList(Seq.init n ((+) 1))
    
    let mutable current = linkedList.First
    while linkedList.Count > 1 do
        linkedList.Remove(nextOrLast current)
        current <- nextOrLast current
    current.Value

elfShuffle1 5
elfShuffle1 6
elfShuffle1 3017957

let elfShuffle2 n =
    let linkedList = System.Collections.Generic.LinkedList(Seq.init n ((+) 1))
    let mutable current = linkedList.First
    let mutable remove =  linkedList.First
    for _ in 1 .. n / 2 do
        remove <- remove |> nextOrLast
    let mutable skipTimes = n % 2 + 1
    while linkedList.Count > 1 do
        let nextRemove = remove
        for n in 1 .. skipTimes do
            remove <- nextOrLast remove
        skipTimes <- if skipTimes = 2 then 1 else 2
        linkedList.Remove(nextRemove)
        current <- nextOrLast current
    current.Value

elfShuffle2 3017957

//Apparently finger trees 