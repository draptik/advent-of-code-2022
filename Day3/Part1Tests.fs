module Part1Tests

open Xunit
open Swensen.Unquote


[<Literal>]
let input = "../../../input.txt"

let sampleData =
    [
        "vJrwpWtwJgWrhcsFMMfFFhFp"
        "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
        "PmmdzqPrVvPwwTWBwg"
        "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
        "ttgJtRGJQctTZtZT"
        "CrZsJsPPZsGzwwsLwLmpwMDw"
    ]

type Compartment = Content of char list

type Rucksack = {
    Compartment1: Compartment
    Compartment2: Compartment
}

let convert' c =
    let inverted =
        let diff = (int 'a') - (int 'Z')
        if System.Char.IsUpper c then
            let i = System.Char.ToLower c |> int
            i - diff + 1
        else
            System.Char.ToUpper c |> int
        
    inverted - ('A' |> int) + 1

let convert'' c =
    let upperCaseOffset = 27
    let lowerCaseOffset = 1
    if System.Char.IsUpper c then
        (int c) - (int 'A') + upperCaseOffset
    else
        (int c) - (int 'a') + lowerCaseOffset

let getRucksack (row:string) : Rucksack =
    let length = row.Length
    let chars = row |> Seq.toList
    let c1 = chars[..(length/2-1)]
    let c2 = chars[length/2..]
    {
        Compartment1 = Content c1
        Compartment2 = Content c2
    }
    
let findDuplicateChar rucksack =
    let rec search l1 l2 =
        match l1 with
        | [] -> ' '
        | head::tail ->
            if List.contains head l2 then
                head
            else
                search tail l2
                
    let (Content c1) = rucksack.Compartment1
    let (Content c2) = rucksack.Compartment2
    search c1 c2
            
[<Fact>]
let ``convert char to Rucksack int`` () =
    let cs = ['a';'z';'A';'Z']
    let actual' = cs |> List.map convert'
    let actual'' = cs |> List.map convert''
    let expected = [1;26;27;52]
    
    actual' =! expected 
    actual'' =! expected 
    
[<Fact>]
let ``Find duplicate char experiments`` () =
    let row1 = sampleData[0]
    let length = row1.Length
    let chars = row1 |> Seq.toList
    let c1 = chars[..(length/2-1)]
    let c2 = chars[length/2..]
    
    let found =
        let rec search l1 l2 =
            match l1 with
            | [] -> ' '
            | head::tail ->
                if List.contains head l2 then
                    head
                else
                    search tail l2
        let result = search c1 c2
        result
    
    let actual = found
    let expected = 'p'
    actual =! expected
    
[<Fact>]
let ``Sample data`` () =
    let actual =
        sampleData
        |> List.map getRucksack
        |> List.map findDuplicateChar
        |> List.map convert''
        |> List.sum
        
    let expected = 157
    actual =! expected
    
[<Fact>]
let ``actual data`` () =
    let actual =
        System.IO.File.ReadAllLines(input)
        |> List.ofArray
        |> List.map getRucksack
        |> List.map findDuplicateChar
        |> List.map convert''
        |> List.sum
        
    let expected = 7889
    actual =! expected