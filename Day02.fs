module Day02

open System

type Draw = { Count: int; Color: string }
type Game = { Index: int; Sets: seq<seq<Draw>> }

// Map of all colors and their limits in the bag
let colorLimits = Map [ ("red", 12); ("green", 13); ("blue", 14) ]

// Get a number from the concatenation of all digits in a string
let getNumber str =
    str |> Seq.filter Char.IsDigit |> Array.ofSeq |> String |> int

// Parse a draw string containing the color and count of cubes
let parseDraw (str: String) =
    let split = str.Trim().Split(" ")
    { Count = int split.[0]; Color = split.[1] }

// Parse all draws in a set string
let parseSet (str: String) =
    str.Trim().Split(",")
    |> Seq.map parseDraw
    
// Parse a game string contain the game index and all draw sets
let parseGame (str: String) =
    let split = str.Trim().Split(":")
    {Index=getNumber split.[0]; Sets=split.[1].Split(";") |> Seq.map parseSet}

// Count the number of colored cubes in a set of draws
let countColoredCubes set color =
    set
    |> Seq.filter (fun x -> x.Color = color)
    |> Seq.map (fun x -> x.Count)
    |> Seq.sum

// Determine if the set is possible by checking if any of the colored cubes in the set exceed the color limits
let isSetPossible (set: seq<Draw>) =
    colorLimits.Keys
    |> Seq.map (fun color -> (color, countColoredCubes set color))
    |> Seq.forall (fun (color, count) -> count <= colorLimits[color])

// Determine if a game is possible by checking if all sets are possible
let isGamePossible game =
    game.Sets
    |> Seq.forall isSetPossible

// Get the maximum number of cubes for a specific color amongst a set of draws
let getColoredMax draws color =
    draws
    |> Seq.filter (fun x -> x.Color = color)
    |> Seq.map (fun x -> x.Count)
    |> Seq.max
    
// Get the power of the game by calculating the product of the maximum count of cubes by color
let getPower game =
    let draws = game.Sets |> Seq.concat
    
    colorLimits.Keys
    |> Seq.map (getColoredMax draws)
    |> Seq.fold (fun acc max -> acc * max) 1

// Returns a sequence containing the solution to each part in order.
let solve (input: string) =
    seq {
        let gameStrings = input.Split("\r\n")
        let games = gameStrings |> Seq.map parseGame

        yield games
              |> Seq.filter isGamePossible
              |> Seq.map (fun x -> x.Index)
              |> Seq.sum

        yield games
              |> Seq.map getPower
              |> Seq.sum
    }
