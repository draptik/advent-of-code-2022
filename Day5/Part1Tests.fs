module Part1Tests

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

type CrateId = char
type Crate = CrateId list
type StackId = int
type Stack = {
    Number: StackId
    Crates: Crate list
}

let getHeader data = data |> List.takeWhile (fun x -> x <> "")
        
/// Examples:
/// 
/// 1 -> 1
/// 2 -> 5
/// 3 -> 9
/// 4 -> 13
/// 5 -> 17
/// 6 -> 21
let indexOfStackNumber n = (n - 1) * 3 + n

/// Extract max number of stacks
let getNumberOfStacks header =

    let bottomRow : string =
        header
        |> List.rev
        |> List.head
        
    let numberOfStacks =
        bottomRow.Trim().Split ' '
        |> Array.rev
        |> Array.head
        |> int
    
    numberOfStacks
    
    
[<Fact>]
let ``Playground - get starting stack of crates`` () =
    
    let header = getHeader sampleData

    let numberOfStacks = getNumberOfStacks header
    
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