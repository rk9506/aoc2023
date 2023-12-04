open System
open System.Text.RegularExpressions

let lines = IO.File.ReadAllLines "./day1input.txt"

// Part 1

let isDigit (c: char) = Int32.TryParse (string c) |> fst

let getCalValPart1 (line: string) = string (Seq.find isDigit line) + string (Seq.findBack isDigit line) |> int

let part1CalValSum = lines |> Seq.map getCalValPart1 |> Seq.sum

Console.WriteLine(part1CalValSum)

// Part 2

let digitWords = [
    "one";
    "two";
    "three";
    "four";
    "five";
    "six";
    "seven";
    "eight";
    "nine"
]


let digitWordPairs = Seq.zip digitWords (seq { 1 .. 9 })
let digitPairs = seq { for i in 1 .. 9 do (string i, i) }
let allPairs = Seq.append digitWordPairs digitPairs
let matchLookup = Map allPairs

let findMatches (needle: string) (haystack: string) = 
    Seq.map 
        (fun (m: Match) -> (needle, m.Index))
        (Seq.cast (Regex(needle).Matches(haystack)))
        

type Term = string
type Index = int
type MatchIndices = (Term * Index) seq

let getMatchIndices (line: string): MatchIndices = 
    Map.keys matchLookup 
    |> Seq.map (fun digit -> findMatches digit line)
    |> Seq.concat
    |> Seq.filter (fun (_, i) -> i <> -1)

let getDigit (matchIndices: MatchIndices) (findBy) = matchIndices |> findBy snd |> fst |> (fun m -> Map.find m matchLookup) |> string
let getCombinedDigits (matchIndices: MatchIndices) = (getDigit matchIndices Seq.minBy) + (getDigit matchIndices Seq.maxBy)

let getCalValPart2 (line: string) = line |> getMatchIndices |> getCombinedDigits |> int

let part2CalValSum = lines |> Seq.map getCalValPart2 |> Seq.sum

Console.WriteLine(part2CalValSum)
