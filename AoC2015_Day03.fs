module AoC2015.Day03

type Position = {X : int; Y : int}
type PositionSet = Set<Position>
type State = {Position : Position; Set : PositionSet}
type RoboState = {Active : int; Positions : Position[]; Set : PositionSet}

let moveUp p = {p with Y = p.Y - 1}

let moveRight p = {p with X = p.X + 1}

let moveDown p = {p with Y = p.Y + 1}

let moveLeft p = {p with X = p.X - 1}

let move direction p =
    match direction with
    | '^' -> moveUp p 
    | '>' -> moveRight p
    | 'v' -> moveDown p
    | '<' -> moveLeft p
    | _   -> p
    
let deliver (path : string) =
    let initialState = {
        Position = {X = 0; Y = 0};
        Set = set [{X = 0; Y = 0}]
    }
    
    let accumulator (state : State) (direction : char) =
        let newPosition = move direction state.Position
        { Position = newPosition; Set = state.Set.Add newPosition }
    
    let sizeOfState (state : State) = state.Set.Count
    
    path.ToCharArray()
    |> Seq.fold accumulator initialState
    |> sizeOfState 
    
let robotDeliver (path : string) =
    let initialState = {
        Active = 0;
        Positions = [| {X = 0; Y = 0}; {X = 0; Y = 0} |];
        Set = set [{X = 0; Y = 0}]
    }
    
    let accumulator (state : RoboState) (direction : char) =
        let newPosition = move direction state.Positions.[state.Active]
        match state.Active with
        | 0 ->
            {
                Active = 1;
                Positions = [| newPosition; state.Positions.[1] |];
                Set = state.Set.Add(newPosition)
            }
        | 1 ->
            {
                Active = 0;
                Positions = [| state.Positions.[0]; newPosition |];
                Set = state.Set.Add(newPosition)
            }
            
    let sizeOfState (state : RoboState) = state.Set.Count
        
    path.ToCharArray()
    |> Seq.fold accumulator initialState
    |> sizeOfState
    
let readLines (filePath : string) =
    System.IO.File.ReadLines(filePath)
    
let part1 input =
    deliver input
    |> printfn "AOC2015 - Day03 - Part 1: %d"
    
let part2 input =
    robotDeliver input
    |> printfn "AOC2015 - Day03 - Part 2: %d"
    
let run =
    let input = readLines "input/aoc2015_day03.txt" |> Seq.head
    part1 input
    part2 input

open NUnit.Framework
open FsUnit

[<Test>]
let ``Part 1 - Example 1`` () =
    deliver ">" |> should equal 2
    
[<Test>]
let ``Part 1 - Example 2`` () =
    deliver "^>v<" |> should equal 4
    
[<Test>]
let ``Part 1 - Example 3`` () =
    deliver "^v^v^v^v^v" |> should equal 2

[<Test>]
let ``Part 2 - Example 1`` () =
    robotDeliver "^v" |> should equal 3
    
[<Test>]
let ``Part 2 - Example 2`` () =
    robotDeliver "^>v<" |> should equal 3
    
[<Test>]
let ``Part 2 - Example 3`` () =
    robotDeliver "^v^v^v^v^v" |> should equal 11
