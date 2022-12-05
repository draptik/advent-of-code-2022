module Part1Tests

open System
open System.Text.RegularExpressions
open Xunit
open Swensen.Unquote

let sampleData =
    [
        "    [D]    "
        "[N] [C]    "
        "[Z] [M] [P]"
        " 1   2   3 "
        ""
        "move 1 from 2 to 1"
        "move 3 from 1 to 3"
        "move 2 from 2 to 1"
        "move 1 from 1 to 2"
    ]

    
/// Examples:
/// 
/// 1 -> 1
/// 2 -> 5
/// 3 -> 9
/// 4 -> 13
/// 5 -> 17
/// 6 -> 21
let indexOfStackNumber n = (n - 1) * 3 + n

let getStackIds header =

    let bottomRow : string =
        header
        |> List.rev
        |> List.head
        
    let numberOfStacks =
        bottomRow.Trim().Split ' '
        |> Array.rev
        |> Array.head
        |> int
    
    [1..numberOfStacks]

type CrateId = char
type Crate = CrateId list
type StackId = int
type Stack = {
    Number: StackId
    Crates: Crate list
}

let tryGetStack (c:char) : char option =
    match c with
    | Char.IsUpper x -> Some x
    | _ -> None

let tryGetStack' (c:char) : char option =
    match c with
    | x when Char.IsUpper x -> Some x
    | _ -> None

let tryGetStack'' (c:char) : char option =
    match Char.IsUpper c with
    | true -> Some c
    | _ -> None
    
let getHeader data = data |> List.takeWhile (fun x -> x <> "")

let addCratesToStack (previousStacks: Stack list) (row: string) : Stack list =
    let cs = row |> List.ofSeq
    
    
    // previousStacks
    // |> List.map (fun stack -> stack.Number)
    // |> List.map indexOfStackNumber
    // |> List.map (fun stackId ->
    //     let value = cs[stackId]
    //     
    previousStacks
    
    
[<Fact>]
let ``Playground - get starting stack of crates`` () =
    
    let header = getHeader sampleData

    let stackIds = getStackIds header
    
    let invertedStackRows =
        header
        |> List.rev
        |> List.tail
    //
    // let cs0 = invertedStackRows[0] |> List.ofSeq
    // let index1 = indexOfStackNumber 3
    // let s1 = cs0[index1]
    
    // invertedStackRows
    // |> List.map
        
    true =! true