module Part1Tests

open System
open Xunit

let sampleData =
    [
        "A Y"
        "B X"
        "C Z"
    ]



[<Fact>]
let ``Sample data`` () =
    let rawInput = sampleData
    
    Assert.True(true)