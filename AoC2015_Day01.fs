module AoC2015.Day01

open NUnit.Framework
open FsUnit

let rules floor x =
    match x with
    | '(' -> floor + 1
    | ')' -> floor - 1
    | _   -> floor

let walk (path : string) =
    path.ToCharArray()
    |> Seq.fold rules 0       

let findFloor (path : string) (floor : int) =
    path.ToCharArray()
    |> Seq.scan rules 0
    |> Seq.takeWhile (( <> ) floor)
    |> Seq.length

let readLines (filePath : string) =
    System.IO.File.ReadLines(filePath)

let part1 input =
    walk input
    |> printfn "AOC2015 - Part 1: %d"

let part2 input =
    findFloor input -1
    |> printfn "AOC2015 - Part 2: %d"

let run = 
    let input = readLines "input/aoc2015_day01.txt" |> Seq.head
    part1 input
    part2 input

[<Test>]
let ``Part 1 example 1`` () =
    walk "(())" |> should equal 0
    
[<Test>]
let ``Part 1 example 2`` () =   
    walk "()()" |> should equal 0
    
[<Test>]
let ``Part 1 example 3`` () =
    walk "(((" |> should equal 3
    
[<Test>]
let ``Part 1 example 4`` () =
    walk "(()(()(" |> should equal 3
    
[<Test>]
let ``Part 1 example 5`` () =
    walk "))(((((" |> should equal 3
    
[<Test>]
let ``Part 1 example 6`` () =
    walk "())" |> should equal -1
    
[<Test>]
let ``Part 1 example 7`` () =
    walk "))(" |> should equal -1
    
[<Test>]
let ``Part 1 example 8`` () =
    walk ")))" |> should equal -3
    
[<Test>]
let ``Part 1 example 9`` () =
    walk ")())())" |> should equal -3

[<Test>]
let ``Part 2 example 1`` () =
    findFloor ")" -1 |> should equal 1
    
[<Test>]
let ``Part 2 example 2`` () =
    findFloor "()())" -1 |> should equal 5
