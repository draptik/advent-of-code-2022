module Part1Tests

open Xunit
open Swensen.Unquote

[<Literal>]
let input = "../../../input.txt"

/// 1st value -> Opponent's selection
/// 2nd value -> proposed selection
let sampleData =
    [
        "A Y"
        "B X"
        "C Z"
    ]

type RPS = | Rock | Paper | Scissors
type State = | Win | Loose | Tie
type Round = {
    Player: RPS
    Opponent: RPS
}

let tryToRound (s:string) =
    let opponentVal = s[0]
    let playerVal = s[2]
    let tryToRpc c =
        if c = 'A' || c = 'X' then Some Rock
        else if c = 'B' || c = 'Y' then Some Paper
        else if c = 'C' || c = 'Z' then Some Scissors
        else None
    match (playerVal |> tryToRpc), (opponentVal |> tryToRpc) with
    | None, _ -> None
    | _, None -> None
    | Some player, Some opponent ->
        {
            Player = player
            Opponent = opponent
        } |> Some

let calcRound (round:Round) =
    let calc first second =
        let roundState =
            match first, second with
            | Rock, Rock -> Tie
            | Rock, Paper -> Loose
            | Rock, Scissors -> Win
            | Paper, Rock -> Win
            | Paper, Paper -> Tie
            | Paper, Scissors -> Loose
            | Scissors, Rock -> Loose
            | Scissors, Paper -> Win
            | Scissors, Scissors -> Tie
        let selection =
            match first with
            | Rock -> 1
            | Paper -> 2
            | Scissors -> 3
        match roundState with
        | Loose -> selection + 0
        | Tie -> selection + 3
        | Win -> selection + 6
        
    calc round.Player round.Opponent

[<Fact>]
let ``Sample data`` () =
    let actual =
        sampleData
        |> List.choose tryToRound
        |> List.map calcRound
        |> List.sum 
    let expected = 15
    actual =! expected
    
[<Fact>]
let ``actual data`` () =
    let actual =
        System.IO.File.ReadAllLines(input)
        |> List.ofArray
        |> List.choose tryToRound
        |> List.map calcRound
        |> List.sum
    let expected = 11386
    actual =! expected
    