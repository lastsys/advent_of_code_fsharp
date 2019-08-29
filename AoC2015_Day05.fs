module AoC2015.Day05
open FsCheck

let containsForbiddenCombinations (s: string) =
    [| "ab"; "cd"; "pq"; "xy" |]
    |> Seq.map s.Contains
    |> Seq.filter id
    |> Seq.length
    |> (<) 0

let containsThreeVowels (s : string) =
    let initialCount =
        Map.ofList [ ('a', 0); ('e', 0); ('i', 0); ('o', 0); ('u', 0) ]
    
    let counter (state : Map<char, int>) (c : char) =
        if state.ContainsKey c
        then state.Add (c, (state.Item c) + 1)
        else state
   
    s.ToCharArray()
    |> Seq.fold counter initialCount
    |> Map.fold (fun s _ v -> s + v) 0
    |> (<=) 3
    
let containsAtLeastOneLetterTwiceInARow (s: string) =
    Seq.zip s (s.Substring 1)
    |> Seq.map (fun (c1, c2) -> c1 = c2)
    |> Seq.filter id
    |> Seq.length
    |> (<=) 1

let containsTwoLettersTwiceWithoutOverlapping (s : string) =
    seq {
        for i in 0 .. (s.Length - 4) do
            for j in i + 2 .. (s.Length - 2) do
                yield s.[i..i+1] = s.[j..j+1]
    }
    |> Seq.filter id
    |> Seq.length
    |> (<=) 1
        
let containsRepeatingLetterWithExactlyOneInBetween (s : string) =
    Seq.zip s (s.Substring 2)
    |> Seq.filter (fun (c1, c2) -> c1 = c2)
    |> Seq.length
    |> (<=) 1
   
let isNice1 (s : string) =
    containsForbiddenCombinations s |> not &&
    containsThreeVowels s &&
    containsAtLeastOneLetterTwiceInARow s

let isNice2 (s : string) =
    containsTwoLettersTwiceWithoutOverlapping s &&
    containsRepeatingLetterWithExactlyOneInBetween s

let readLines (filePath : string) =
    System.IO.File.ReadLines(filePath)

let part1 input =
    Seq.map isNice1 input
    |> Seq.filter id
    |> Seq.length
    |> printfn "AOC2015 - Day05 - Part 1: %d"

let part2 input =
    Seq.map isNice2 input
    |> Seq.filter id
    |> Seq.length
    |> printfn "AOC2015 - Day05 - Part 2: %d"
    
let run =
    let input = readLines "input/aoc2015_day05.txt"
    part1 input
    part2 input

open NUnit.Framework
open FsUnit

[<Test>]
let ``Part 1 - Contains three vowels`` () =
    containsThreeVowels "aei" |> should equal true
    containsThreeVowels "xazegov" |> should equal true
    containsThreeVowels "aeiouaeiouaeiou" |> should equal true
    
[<Test>]
let ``Part 1 - At least one letter twice in a row`` () =
    containsAtLeastOneLetterTwiceInARow "xx" |> should equal true
    containsAtLeastOneLetterTwiceInARow "abcdde" |> should equal true
    containsAtLeastOneLetterTwiceInARow "aabbccdd" |> should equal true

[<Test>]
let ``Part 1 - Does not contain invalid strings`` () =
    containsForbiddenCombinations "argh" |> should equal false
    containsForbiddenCombinations "markabcde" |> should equal true

[<Test>]
let ``Part 1 - Example 1`` () =
    isNice1 "ugknbfddgicrmopn" |> should equal true
    
[<Test>]
let ``Part 1 - Example 2`` () =
    isNice1 "aaa" |> should equal true
    
[<Test>]
let ``Part 1 - Example 3`` () =
    isNice1 "jchzalrnumimnmhp" |> should equal false
    
[<Test>]
let ``Part 1 - Example 4`` () =
    isNice1 "haegwjzuvuyypxyu" |> should equal false
    
[<Test>]
let ``Part 1 - Example 5`` () =
    isNice1 "dvszwmarrgswjxmb" |> should equal false
   
[<Test>]
let ``Part 2 - Two letters appears twice without overlapping - xyxy`` () =
    containsTwoLettersTwiceWithoutOverlapping "xyxy" |> should equal true

[<Test>]
let ``Part 2 - Two letters appears twice without overlapping - aabcdefgaa`` () =
    containsTwoLettersTwiceWithoutOverlapping "aabcdefgaa" |> should equal true

[<Test>]
let ``Part 2 - Two letters appears twice without overlapping - aaa`` () =
    containsTwoLettersTwiceWithoutOverlapping "aaa" |> should equal false

[<Test>]
let ``Part 2 - One letter repeats with exactly one letter in-between - xyx`` () =
    containsRepeatingLetterWithExactlyOneInBetween "xyx" |> should equal true

[<Test>]
let ``Part 2 - One letter repeats with exactly one letter in-between - abcdefeghi`` () =
    containsRepeatingLetterWithExactlyOneInBetween "abcdefeghi" |> should equal true

[<Test>]
let ``Part 2 - One letter repeats with exactly one letter in-between - efe`` () =
    containsRepeatingLetterWithExactlyOneInBetween "efe" |> should equal true

[<Test>]
let ``Part 2 - One letter repeats with exactly one letter in-between - aaa`` () =
    containsRepeatingLetterWithExactlyOneInBetween "aaa" |> should equal true

[<Test>]
let ``Part 2 - One letter repeats with exactly one letter in-between - xx`` () =
    containsRepeatingLetterWithExactlyOneInBetween "xx" |> should equal false

[<Test>]
let ``Part 2 - Example 1`` () =
    isNice2 "qjhvhtzxzqqjkmpb" |> should equal true

[<Test>]
let ``Part 2 - Example 2`` () =
    isNice2 "xxyxx" |> should equal true

[<Test>]
let ``Part 2 - Example 3`` () =
    isNice2 "uurcxstgmygtbstg" |> should equal false
    
[<Test>]
let ``Part 2 - Example 3 - Has Pair`` () =
    containsTwoLettersTwiceWithoutOverlapping "uurcxstgmygtbstg" |> should equal true
    
[<Test>]
let ``Part 2 - Example 3 - No repeat with single letter in between`` () =
    containsRepeatingLetterWithExactlyOneInBetween "uurcxstgmygtbstg" |> should equal false
    
[<Test>]
let ``Part 2 - Example 4`` () =
    isNice2 "ieodomkazucvgmuy" |> should equal false

[<Test>]
let ``Part 2 - Example A`` () =
    isNice2 "xilodxfuxphuiiii" |> should equal true

[<Test>]
let ``Part 2 - Example A - Contains two letters twice without overlapping`` () =
    containsTwoLettersTwiceWithoutOverlapping "xilodxfuxphuiiii" |> should equal true

[<Test>]
let ``Part 2 - Example A - Contains repeating letter with exactly one in between`` () =
    containsRepeatingLetterWithExactlyOneInBetween "xilodxfuxphuiiii" |> should equal true

[<Test>]
let ``Part 2 - Example B`` () =
    isNice2 "pzkkkkwrlvxiuysn" |> should equal true

[<Test>]
let ``Part 2 - Example C`` () =
    isNice2 "pzkkkkwrlvxiuysn" |> should equal true


[<Test>]
let ``Part 2 - Example D`` () =
    isNice2 "bkkkkcwegvypbrio" |> should equal true

