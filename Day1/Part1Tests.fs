module Part1Tests

open Xunit
open Swensen.Unquote

[<Literal>]
let input = "../../../input.txt"

[<Fact>]
let ``Input has correct number of lines`` () =
    let lines = System.IO.File.ReadAllLines(input)
    let actual = lines.Length
    let expected = 2255
    expected =! actual

let smallSampleData =
    [
        "1000"
        "2000"
        "3000"
        ""
        "4000"
        ""
        "5000"
        "6000"
        ""
        "7000"
        "8000"
        "9000"
        ""
        "10000"
    ]


/// http://www.fssnip.net/nr/title/Split-a-list-using-a-separator
/// Split a list into chunks using the specified separator
/// This takes a list and returns a list of lists (chunks)
/// that represent individual groups, separated by the given
/// separator 'v'
let splitBy v list =
  let yieldRevNonEmpty list = 
    if list = [] then []
    else [List.rev list]

  let rec loop groupSoFar list = seq { 
    match list with
    | [] -> yield! yieldRevNonEmpty groupSoFar
    | head::tail when head = v ->
        yield! yieldRevNonEmpty groupSoFar
        yield! loop [] tail
    | head::tail ->
        yield! loop (head::groupSoFar) tail }
  loop [] list |> List.ofSeq


[<Fact>]
let ``Chunking`` () =
    let entries = smallSampleData
    let seperator = ""
    let actual = splitBy seperator entries
    let expected = [["1000"; "2000"; "3000"]; ["4000"]; ["5000"; "6000"]; ["7000"; "8000"; "9000"]; ["10000"]]
    expected =! actual


[<Fact>]
let ``Sample data parsing works`` () =
    let entries = smallSampleData
    let seperator = ""
    let actual =
        splitBy seperator entries
        |> List.map(fun x -> x |> List.map(fun y -> y |> int))
        |> List.map(fun x -> x |> List.sum)
        |> List.max
    
    let expected = 24000
    expected =! actual


[<Fact>]
let ``Input data parsing works`` () =
    let entries = System.IO.File.ReadAllLines(input) |> List.ofSeq
    let seperator = ""
    let actual =
        splitBy seperator entries
        |> List.map(fun elf -> 
            elf 
            |> List.map(fun y -> y |> int) 
            |> List.sum)
        |> List.max
    
    let expected = 71934
    expected =! actual
