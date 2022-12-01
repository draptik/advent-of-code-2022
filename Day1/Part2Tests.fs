module Part2Tests

open Xunit
open Swensen.Unquote

open Part1Tests

[<Fact>]
let ``Input data parsing works - sum of top 3 elves`` () =
    let entries = System.IO.File.ReadAllLines(input) |> List.ofSeq
    let seperator = ""
    let actual =
        splitBy seperator entries
        |> List.map(fun elf -> 
            elf 
            |> List.map(fun y -> y |> int) 
            |> List.sum)
        |> List.sort
        |> List.rev
        |> List.take 3
        |> List.sum
    
    let expected = 211447
    expected =! actual
