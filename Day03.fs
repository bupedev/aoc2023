module Day03

open System
open System.Text.RegularExpressions

// Represents the size and location of a number in text
type Number = { Row: int; Column: int; Length: int; Value: int }

// Represents the location of a symbol in text
type Symbol = { Row: int; Column: int; Value: char }

// Get all numbers in a string in a particular line
let getNumbers (line: int) (str: string) =
    Regex.Matches(str, @"\d+")
    |> Seq.map (fun number ->
        { Row = line
          Column = number.Index
          Length = number.Length
          Value = int number.Value })

// Get all symbols in a string on a particular line
let getSymbols (line: int) (str: string) =
    str
    |> Seq.mapi (fun i x -> (i, x))
    |> Seq.filter (fun (_, x) -> x <> '.' && not (Char.IsDigit(x)))
    |> Seq.map (fun (i, x) -> { Row = line; Column = i; Value = x })

// Determine if a symbol is adjacent to a number
let isAdjacent (number: Number) (symbol: Symbol) =
    symbol.Row >= number.Row - 1
    && symbol.Row <= number.Row + 1
    && symbol.Column >= number.Column - 1
    && symbol.Column <= number.Column + number.Length

// Determine if a number is adjacent to any of the specified symbols
let isAdjacentToAnySymbol (symbols: seq<Symbol>) (number: Number) =
    (symbols |> Seq.filter (isAdjacent number) |> Seq.length) > 0

// Calculate the product of sequence of integers 
let product values =
    values |> Seq.fold (fun acc value -> acc * value) 1

// Calculate the gear ratio of a gear symbol, returns 0 if the gear doesn't have two adjacent numbers
let getGearRatio (numbers: seq<Number>) (gear: Symbol) =
    let adjacent =
        numbers
        |> Seq.filter (fun number -> isAdjacent number gear)
        |> Seq.map (fun x -> x.Value)
        |> Array.ofSeq

    if (Seq.length adjacent = 2) then product adjacent else 0

// Returns a sequence containing the solution to each part in order.
let solve (input: string) =
    seq {
        let lines = input.Split("\r\n") |> Array.ofSeq
        let numbers = lines |> Seq.mapi getNumbers |> Seq.concat
        let symbols = lines |> Seq.mapi getSymbols |> Seq.concat

        yield
            numbers
            |> Seq.filter (isAdjacentToAnySymbol symbols)
            |> Seq.map (fun x -> x.Value)
            |> Seq.sum

        let gears = symbols |> Seq.filter (fun x -> x.Value = '*')

        yield gears |> Seq.map (getGearRatio numbers) |> Seq.sum
    }
