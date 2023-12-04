module Day04

open System
open System.Linq

// Represents the numbers on the scratch card
type Card =
    { WinningNumbers: Set<int>
      DrawnNumbers: Set<int> }

// Parse a set of space delimited numbers from a string
let parseNumberSet (str: String) =
    str.Trim().Split(" ", StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map int
    |> Set.ofSeq

// Parse a card from a line
let parseCard (line: String) =
    let numberSplit = line.Split(":").[1].Split("|")

    { WinningNumbers = parseNumberSet numberSplit.[0]
      DrawnNumbers = parseNumberSet numberSplit.[1] }

// Get the number of winning number matches on a card
let getMatchCount (card: Card) =
    Set.intersect card.WinningNumbers card.DrawnNumbers |> Seq.length

// Get the number of false points for card 
let getFalsePoints (card: Card) =
    let matchCount = getMatchCount card

    if matchCount > 0 then
        int (Math.Pow(2.0, float matchCount - 1.0))
    else
        0

// Get the number of true points for a card (including points of copies it creates)
let rec getTruePoints (cards: array<Card>) index =
    let matchCount = getMatchCount cards.[index]

    let downstreamCardCount =
        Enumerable.Range(index + 1, matchCount)
        |> Seq.map (getTruePoints cards)
        |> Seq.sum

    downstreamCardCount + 1

// Returns a sequence containing the solution to each part in order.
let solve (input: string) =
    seq {
        let cards = input.Split(Environment.NewLine) |> Seq.map parseCard |> Array.ofSeq

        yield cards |> Seq.map getFalsePoints |> Seq.sum

        yield cards |> Seq.mapi (fun i _ -> getTruePoints cards i) |> Seq.sum
    }
