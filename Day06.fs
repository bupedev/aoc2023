module Day06

open System

type Range = { Lower: int; Upper: int }
type Game = { Time: float; Distance: float }

// Parse all of the games assuming they are space delimited
let parseGamesWithSpaces (lines: seq<string>) =
    let parsed =
        lines
        |> Seq.map (fun line -> line.Split(":").[1])
        |> Seq.map (fun line -> line.Trim().Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Seq.map float)

    Seq.zip (Seq.head parsed) (Seq.last parsed)
    |> Seq.map (fun (time, distance) -> { Time = time; Distance = distance })

// Parse the single game as a sequence assuming that there is random spacing provided
let parseGamesWithoutSpaces (lines: seq<string>) =
    let parsed =
        lines
        |> Seq.map (fun line -> line.Split(":").[1])
        |> Seq.map (fun line ->
            line.Trim().Split(" ", StringSplitOptions.RemoveEmptyEntries)
            |> Seq.concat
            |> Array.ofSeq
            |> String
            |> float)

    seq {
        yield
            { Time = (Seq.head parsed)
              Distance = (Seq.last parsed) }
    }

// Calculate the range of winning strategies using some high-school quadratic math
let calculateWinningStrategyRange game =
    let time = float game.Time
    let distance = float game.Distance

    let disc = sqrt ((time ** 2.0) - 4.0 * distance)
    let lower = floor ((time - disc) / 2.0 + 1.0) |> int
    let upper = ceil ((time + disc) / 2.0 - 1.0) |> int

    { Lower = lower; Upper = upper }

// Calculate the product of the count of winning strategies across all games
let calculateWinningStrategyProduct games =
    games
    |> Seq.map calculateWinningStrategyRange
    |> Seq.map (fun range -> range.Upper - range.Lower + 1)
    |> Seq.fold (fun acc value -> acc * value) 1

// Returns a sequence containing the solution to each part in order
let solve (input: string) =
    seq {
        let lines = input.Split(Environment.NewLine)

        yield parseGamesWithSpaces lines |> calculateWinningStrategyProduct
        yield parseGamesWithoutSpaces lines |> calculateWinningStrategyProduct
    }
