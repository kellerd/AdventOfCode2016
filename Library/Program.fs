namespace Advent

module Library = 
    let test f textInstructions = f textInstructions
    
    let is expectedResult result = 
        if result <> expectedResult then 
            printfn "Got %A, expected %A" result expectedResult
            false
        else 
            printfn "It worked: %A" result
            true
