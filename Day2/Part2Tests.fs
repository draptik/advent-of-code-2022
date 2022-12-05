module Part2Tests

open Xunit
open Swensen.Unquote

open Part1Tests

type Round' = {
    State: State
    Opponent: RPS
}

let tryToRound' (s:string) =
    let opponentVal = s[0]
    let stateVal = s[2]
    let tryToRpc c =
        if c = 'A' then Some Rock
        else if c = 'B' then Some Paper
        else if c = 'C' then Some Scissors
        else None
    let tryToState c =
        if c = 'X' then Some Loose
        else if c = 'Y' then Some Tie
        else if c = 'Z' then Some Win
        else None
    match (stateVal |> tryToState), (opponentVal |> tryToRpc) with
    | None, _ -> None
    | _, None -> None
    | Some state, Some opponent ->
        {
            State = state
            Opponent = opponent
        } |> Some

let calcSelection (round:Round') =
    let calc opponent state =
        let roundSelection =
            match opponent, state with
            | Rock, Loose -> Scissors
            | Rock, Tie -> Rock
            | Rock, Win -> Paper
            | Paper, Loose -> Rock
            | Paper, Tie -> Paper
            | Paper, Win -> Scissors
            | Scissors, Loose -> Paper
            | Scissors, Tie -> Scissors
            | Scissors, Win -> Rock
        let currentState =
            match state with
            | Loose -> 0
            | Tie -> 3
            | Win -> 6
        match roundSelection with
        | Rock -> currentState + 1
        | Paper -> currentState + 2
        | Scissors -> currentState + 3
        
    calc round.Opponent round.State

[<Fact>]
let ``Sample data`` () =
    let actual =
        sampleData
        |> List.choose tryToRound'
        |> List.map calcSelection
        |> List.sum 
    let expected = 12
    actual =! expected
    
[<Fact>]
let ``actual data`` () =
    let actual =
        System.IO.File.ReadAllLines(input)
        |> List.ofArray
        |> List.choose tryToRound'
        |> List.map calcSelection
        |> List.sum
    let expected = 13600
    actual =! expected
