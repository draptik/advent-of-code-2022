module Part1Tests

open System.Text.RegularExpressions
open Xunit
open Swensen.Unquote

[<Literal>]
let input = "../../../input.txt"

let sampleData =
    [
        "2-4,6-8"
        "2-3,4-5"
        "5-7,7-9"
        "2-8,3-7"
        "6-6,4-6"
        "2-6,4-8"
    ]

    
[<Fact>]
let ``Playground 0`` () =
    let l1 = [1;5]
    let l2 = [2;5]

    let check =
        if l1[0] >= l2[0] && l1[1] <= l2[1] then true
        else l2[0] >= l1[0] && l2[1] <= l1[1]
        
    check =! true
    
    
[<Fact>]
let ``Playground 1`` () =
    
    let row1 = sampleData[0]
    let foo = row1.Split ','
    let pair1 = foo[0]
    let pair2 = foo[1]
    let l1 = pair1.Split '-'
    let l2 = pair2.Split '-'
    
    let actual =
        if l1[0] >= l2[0] && l1[1] <= l2[1] then true
        else l2[0] >= l1[0] && l2[1] <= l1[1]
    
    
    let expected = false
    actual =! expected

[<Fact>]
let ``Playground 2`` () =
    
    let row1 = sampleData[0]
    let pattern = @"(?<s1>\d+)-(?<e1>\d+),(?<s2>\d+)-(?<e2>\d+)"
    let reg = Regex(pattern, RegexOptions.Compiled)
    let matches = reg.Match row1
    let s1 = matches.Groups["s1"] |> string |> int
    let s2 = matches.Groups["s2"] |> string |> int
    let e1 = matches.Groups["e1"] |> string |> int
    let e2 = matches.Groups["e2"] |> string |> int
    
    let actual =
        if s1 >= s2 && e1 <= e2 then true
        else s2 >= s1 && e2 <= e1
    
    
    let expected = false
    actual =! expected
    
   
[<Fact>]
let ``sample data`` () =
    
    let isContained row =
        let pattern = @"(?<s1>\d+)-(?<e1>\d+),(?<s2>\d+)-(?<e2>\d+)"
        let reg = Regex(pattern, RegexOptions.Compiled)
        let matches = reg.Match row
        let s1 = matches.Groups["s1"] |> string |> int
        let s2 = matches.Groups["s2"] |> string |> int
        let e1 = matches.Groups["e1"] |> string |> int
        let e2 = matches.Groups["e2"] |> string |> int
    
        if s1 >= s2 && e1 <= e2 then true
        else s2 >= s1 && e2 <= e1
    
    let actual =
        sampleData
        |> List.map isContained
        |> List.filter id
        |> List.length
    
    let expected = 2
    actual =! expected
       
[<Fact>]
let ``actual data`` () =
    
    let isContained row =
        let pattern = @"(?<s1>\d+)-(?<e1>\d+),(?<s2>\d+)-(?<e2>\d+)"
        let reg = Regex(pattern, RegexOptions.Compiled)
        let matches = reg.Match row
        let s1 = matches.Groups["s1"] |> string |> int
        let s2 = matches.Groups["s2"] |> string |> int
        let e1 = matches.Groups["e1"] |> string |> int
        let e2 = matches.Groups["e2"] |> string |> int
    
        if s1 >= s2 && e1 <= e2 then true
        else s2 >= s1 && e2 <= e1
    
    let actual =
        System.IO.File.ReadAllLines(input)
        |> List.ofArray
        |> List.map isContained
        |> List.filter id
        |> List.length
    
    let expected = 526
    actual =! expected
    
    