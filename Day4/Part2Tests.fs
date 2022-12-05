module Part2Tests

open System.Text.RegularExpressions
open Xunit
open Swensen.Unquote
open Part1Tests

let hasOverlap row =
    let pattern = @"(?<s1>\d+)-(?<e1>\d+),(?<s2>\d+)-(?<e2>\d+)"
    let reg = Regex(pattern, RegexOptions.Compiled)
    let matches = reg.Match row
    let s1 = matches.Groups["s1"] |> string |> int
    let s2 = matches.Groups["s2"] |> string |> int
    let e1 = matches.Groups["e1"] |> string |> int
    let e2 = matches.Groups["e2"] |> string |> int

    if s1 >= s2 && s1 <= e2 || e1 >= s2 && e1 <= e2 then true
    else s2 >= s1 && s2 <= e1 || e2 >= s1 && e2 <= e1
      
[<Fact>]
let ``sample data`` () =
    let actual =
        sampleData
        |> List.map hasOverlap
        |> List.filter id
        |> List.length
    
    let expected = 4
    actual =! expected
         
[<Fact>]
let ``actual data`` () =
    let actual =
        System.IO.File.ReadAllLines(input)
        |> List.ofArray
        |> List.map hasOverlap
        |> List.filter id
        |> List.length
    
    let expected = 886
    actual =! expected
   