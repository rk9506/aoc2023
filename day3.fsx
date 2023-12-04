open System
open System.Text.RegularExpressions

let lines = IO.File.ReadAllLines "./day3input.txt"

type Point = { X: int; Y: int }
type Span = { Start: Point; End: Point }
type NumberEntry = { Num: int; Span: Span }

let matchToNumberEntry (lineNum: int) (m: Match) = 
    {   Num = (int m.Value);
        Span = {
            Start = { X = m.Index; Y = lineNum };
            End = { X = m.Index + m.Length - 1; Y = lineNum } } }

let getNumberEntries (lines: string array) (lineNum: int) =
    let line = lines[lineNum]
    let matches = Regex("\\d+").Matches(line)

    Seq.map (matchToNumberEntry lineNum) matches

let getRowOfPoints (xStart: int) (xEnd: int) (y: int) = 
    [for x in xStart .. xEnd do { X = x; Y = y }]

let getAdjacentPoints (span: Span): Point list =
    let schematicSize = (lines.Length, lines[0].Length)

    let isAtLeftBorder = span.Start.X = 0
    let isAtRightBorder = span.End.X = (fst schematicSize) - 1
    let xStart = if isAtLeftBorder then span.Start.X else span.Start.X - 1
    let xEnd = if isAtRightBorder then span.End.X else span.End.X + 1

    let y = span.Start.Y
    let topRow = if y = 0 then [] else getRowOfPoints xStart xEnd (y - 1)
    let bottomRow = if y = (snd schematicSize) - 1 then [] else getRowOfPoints xStart xEnd (y + 1)
    let leftPoint = if isAtLeftBorder then [] else [{ X = xStart; Y = y }]
    let rightPoint = if isAtRightBorder then [] else [{ X = xEnd; Y = y }]

    let pts = List.concat [topRow; bottomRow; leftPoint; rightPoint]
    pts

// Part 1

let nonSymbols = set ('.' :: [for i in 0 .. 9 do char (i + int '0')])
let hasAdjacentSymbol (lines: string array) (numEntry: NumberEntry) =
    let adjacentPoints = getAdjacentPoints numEntry.Span
    let isSymbolPoint (p: Point) = not (Set.contains (lines[p.Y][p.X]) nonSymbols)
    List.exists isSymbolPoint adjacentPoints

let getPartNumbers (lines: string array) =
    [0 .. lines.Length - 1]
    |> Seq.map (getNumberEntries lines)
    |> Seq.concat
    |> Seq.filter (hasAdjacentSymbol lines)
    |> Seq.map (fun numEntry -> numEntry.Num)


let partNumberSum = Seq.sum (getPartNumbers lines)

Console.WriteLine partNumberSum

// Part 2

type GearEntry = { Loc: Point; AdjacentNumbers: int list }

let isGearPoint (p: Point) = lines[p.Y][p.X] = '*'

let getAdjacentGears (span: Span) = getAdjacentPoints span |> List.filter isGearPoint

let getGearEntries (numEntries: NumberEntry seq) =
    let gearPoints= 
        numEntries
        |> Seq.map (fun n -> getAdjacentGears n.Span |> List.map (fun g -> (g, n)))
        |> List.concat

    let gearEntries =
        Seq.groupBy fst gearPoints
        |> Seq.map (fun (gearLoc, pairs) ->
            { Loc = gearLoc; AdjacentNumbers = List.map (fun (_, n: NumberEntry) -> n.Num) (List.ofSeq pairs) })

    gearEntries

let getGearRatio (gear: GearEntry) = Seq.head gear.AdjacentNumbers * Seq.item 1 gear.AdjacentNumbers

let getGearRatioSum (lines: string array) =
    [0 .. lines.Length - 1]
    |> Seq.map (getNumberEntries lines)
    |> Seq.concat
    |> getGearEntries
    |> Seq.filter (fun g -> g.AdjacentNumbers.Length = 2)
    |> Seq.map getGearRatio
    |> Seq.sum

let gearRatioSum = getGearRatioSum lines

Console.WriteLine gearRatioSum
