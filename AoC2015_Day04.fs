module AoC2015.Day04

open NUnit.Framework
open FsUnit
open System.Security.Cryptography

let mine (leadingZeroes : int) (secretKey : string) =
    let md5 = MD5.Create()

    let indexOfFirstNonZero (s : string) =
        s.ToCharArray()
        |> Seq.takeWhile ((=) '0')
        |> Seq.length
        
    let rec findNumber (number : int) =
        let hash =
            secretKey + number.ToString()
            |> System.Text.Encoding.ASCII.GetBytes
            |> md5.ComputeHash
            |> Array.map (fun x -> System.String.Format("{0:X2}", x))
            |> String.concat System.String.Empty
        if indexOfFirstNonZero hash = leadingZeroes
        then number
        else findNumber (number + 1)
        
    findNumber 0

let part1 input =
    mine 5 input
    |> printfn "AOC2015 - Day04 - Part 1: %d"

let part2 input =
    mine 6 input
    |> printfn "AOC2015 - Day04 - Part 2: %d"

let run =
    let key = "bgvyzdsv" 
    part1 key
    part2 key

[<Test>]
let ``Part 1 - Example 1`` () =
    mine 5 "abcdef" |> should equal 609043
    
[<Test>]
let ``Part 1 - Example 2`` () =
    mine 5 "pqrstuv" |> should equal 1048970
