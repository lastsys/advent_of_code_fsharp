module AoC2015.Day02

let wrappingPaperNeeded l w h =
    let areas = [| l*w; w*h; h*l |]
    let minimumArea = Seq.min areas
    areas
    |> Seq.sum
    |> ( * ) 2
    |> ( + ) minimumArea

let ribbonLengthNeeded l w h =
    let needed =
        [| l; w; h; |]
        |> Seq.sort
        |> Seq.take 2
        |> Seq.sum
        |> ( * ) 2
    let extra = l * w * h
    needed + extra
    
let readLines (filePath : string) =
    System.IO.File.ReadLines(filePath)
    
let parseLine (line : string) =
    let toTuple (x : int[]) = (x.[0], x.[1], x.[2])
    line.Split("x")
    |> Seq.map int
    |> Seq.toArray
    |> toTuple
    
let part1 input =
    input
    |> Seq.map (fun x -> x |||> wrappingPaperNeeded)
    |> Seq.sum
    |> printfn "AOC2015 - Day02 - Part 1: %d"
    
let part2 input =
    input
    |> Seq.map (fun x -> x |||> ribbonLengthNeeded)
    |> Seq.sum
    |> printfn "AOC2015 - Day02 - Part 2: %d"
    
let run =
    let input =
        readLines "input/aoc2015_day02.txt"
        |> Seq.map parseLine
    part1 input
    part2 input

open NUnit.Framework
open FsUnit

[<Test>]
let ``Part 1 - Example 1`` () =
    wrappingPaperNeeded 2 3 4 |> should equal 58
    
[<Test>]
let ``Part 1 - Example 2`` () =
    wrappingPaperNeeded 1 1 10 |> should equal 43

[<Test>]
let ``Part 2 - Example 1`` () =
    ribbonLengthNeeded 2 3 4 |> should equal 34
    
[<Test>]
let ``Part 2 - Example 2`` () =
    ribbonLengthNeeded 1 1 10 |> should equal 14

[<Test>]
let ``parseLine should give expected result`` () =
    parseLine "2x3x4" |> should equal (2, 3, 4)
    