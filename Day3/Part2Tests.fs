module Part2Tests

open Xunit
open Swensen.Unquote
open Part1Tests

let findTripleChar l1 l2 l3 =
    let rec search l1 l2 l3 =
        match l1 with
        | [] -> ' '
        | head::tail ->
            if List.contains head l2 && List.contains head l3 then
                head
            else
                search tail l2 l3
    search l1 l2 l3


[<Fact>]
let ``find triplets playground`` () =
    let triplets =
        sampleData
        |> List.chunkBySize 3
    
    let firstTriplet = triplets[0]
    
    let x1 = firstTriplet[0] |> Seq.toList
    let x2 = firstTriplet[1] |> Seq.toList
    let x3 = firstTriplet[2] |> Seq.toList
    
    let actual = findTripleChar x1 x2 x3
    let expected = 'r'
    actual =! expected

type Triplet = {
    First: string
    Second: string
    Third: string
}
type SearchableTriplet = {
    First: char list
    Second: char list
    Third: char list
}
let toTriplet (s:string list) : Triplet =
    {
        Triplet.First = s[0]
        Second = s[1]
        Third = s[2]
    }
    
let toSearchableTriplet (triplet:Triplet) =
    {
        SearchableTriplet.First = triplet.First |> Seq.toList
        Second = triplet.Second |> Seq.toList
        Third = triplet.Third |> Seq.toList
    }

let findTripleCharFromSearchableTriplet (searchableTriplet: SearchableTriplet) : char =
    findTripleChar searchableTriplet.First searchableTriplet.Second searchableTriplet.Third
    
[<Fact>]
let ``sample data`` () =
    let actual =
        sampleData
        |> List.chunkBySize 3
        |> List.map toTriplet
        |> List.map toSearchableTriplet
        |> List.map findTripleCharFromSearchableTriplet
        |> List.map convert''
        |> List.sum
    
    let expected = 70
    actual =! expected
        
    
[<Fact>]
let ``actual data`` () =
    let actual =
        System.IO.File.ReadAllLines(input)
        |> List.ofArray
        |> List.chunkBySize 3
        |> List.map toTriplet
        |> List.map toSearchableTriplet
        |> List.map findTripleCharFromSearchableTriplet
        |> List.map convert''
        |> List.sum
    
    let expected = 2825
    actual =! expected
    