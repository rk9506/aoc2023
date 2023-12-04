open System.Text.RegularExpressions

let lines = System.IO.File.ReadAllLines "./day2input.txt"

type Cubes = { Red: int; Green: int; Blue: int }
let initialHandful = { Red = 0; Green = 0; Blue = 0 }

let getHandfuls (line: string) = line.Split ";"

let addToHandful (handful: Cubes) (m: Match)  =
    match m.Groups[2].ToString() with
    | "red" -> { handful with Red = m.Groups[1].ToString() |> int }
    | "blue" -> { handful with Blue = m.Groups[1].ToString() |> int }
    | "green" -> { handful with Green = m.Groups[1].ToString() |> int }
    | _ -> handful

let parseHandful (s: string) = 
    let matches: Match seq = Seq.cast (Regex("(\d+) (red|green|blue)").Matches(s))
    Seq.fold addToHandful initialHandful matches

let printHandful (handful: Cubes) = printfn "Red = %d, Green = %d, Blue = %d" handful.Red handful.Green handful.Blue

let bag = { Red = 12; Green = 13; Blue = 14 }

// Part 1

let isHandfulPossible (handful: Cubes) = handful.Red <= bag.Red && handful.Green <= bag.Green && handful.Blue <= bag.Blue
let isGamePossible (game: string) = game |> getHandfuls |> Seq.map parseHandful |> Seq.forall isHandfulPossible

let gamesWithIds = Seq.zip [1 .. lines.Length] lines

let sumPossibleGameIds = gamesWithIds |> Seq.filter (fun (_, game) -> isGamePossible game) |> Seq.sumBy fst

System.Console.WriteLine sumPossibleGameIds

// Part 2

let getMaxCubes (colourGetter: Cubes -> int) (handfuls: Cubes seq) = handfuls |> Seq.map colourGetter |> Seq.max

let getMinimumBag (handfuls: Cubes seq): Cubes = 
    { Red = handfuls |> getMaxCubes (fun h -> h.Red) 
      Green = handfuls |> getMaxCubes (fun h -> h.Green)
      Blue = handfuls |> getMaxCubes (fun h -> h.Blue) }

let setZeroToOne n = if n = 0 then 1 else n
let getPower (bag: Cubes): int = setZeroToOne bag.Red * setZeroToOne bag.Green * setZeroToOne bag.Blue

let sumMinBagPowers = 
    lines 
    |> Seq.map (fun line -> line |> getHandfuls |> Seq.map parseHandful)
    |> Seq.map (getMinimumBag >> getPower) 
    |> Seq.sum

System.Console.WriteLine sumMinBagPowers