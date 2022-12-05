module Part1Tests

open Xunit
open Swensen.Unquote

[<Literal>]
let input = "../../../input.txt"

let sampleData =
    [
        "A Y"
        "B X"
        "C Z"
    ]

type RPS = | Rock | Paper | Scissors
type Player =
    | PlayerSelf of RPS
    | Opponent of RPS

let calcRound (s:string) : int =
    let opponentVal = s[0]
    let playerVal = s[2]
    let opponent =
        match opponentVal with
        | c when c = 'A' -> Opponent Rock 
        | c when c = 'B' -> Opponent Paper 
        | _ -> Opponent Scissors
    let player =
        match playerVal with
        | c when c = 'X' -> PlayerSelf Rock
        | c when c = 'Y' -> PlayerSelf Paper
        | _ -> PlayerSelf Scissors
        
    let result =
        match player, opponent with
        | PlayerSelf Rock, Opponent Rock -> 1+3
        | PlayerSelf Paper, Opponent Rock -> 2+6
        | PlayerSelf Scissors, Opponent Rock -> 3+0
        | PlayerSelf Rock, Opponent Paper -> 1+0
        | PlayerSelf Paper, Opponent Paper -> 2+3
        | PlayerSelf Scissors, Opponent Paper -> 3+6
        | PlayerSelf Rock, Opponent Scissors -> 1+6
        | PlayerSelf Paper, Opponent Scissors -> 2+0
        | PlayerSelf Scissors, Opponent Scissors -> 3+3
        | _ -> 0
    result
    
let calcTotal xs =
    xs |> List.map calcRound |> List.sum

[<Fact>]
let ``Sample data`` () =
    let actual =
        sampleData
        |> calcTotal 
    let expected = 15
    actual =! expected
    
[<Fact>]
let ``actual data`` () =
    let lines = System.IO.File.ReadAllLines(input)
    let actual =
        lines
        |> List.ofArray
        |> calcTotal 
    let expected = 15 // 11386
    actual =! expected
    