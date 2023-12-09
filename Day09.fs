module Day09

open System

// Parse a history string as a sequence of integers
let parseHistory (line: string) =
    line.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Seq.map int

// Recursively get the differences for the history, stopping only when we encounter all zeroes
let rec getDifferences sequence =
    if sequence |> Seq.exists (fun x -> x <> 0) then
        let next =
            sequence |> Seq.skip 1 |> Seq.zip sequence |> Seq.map (fun (a, b) -> b - a)

        sequence :: getDifferences next
    else
        []

// Calculate the first value after the history using the differences
let getNextValue differences =
    differences |> Seq.rev |> Seq.map Seq.last |> Seq.fold (fun acc x -> x + acc) 0

// Calculate the first value before the history using the differences
let getPreviousValue differences =
    differences |> Seq.rev |> Seq.map Seq.head |> Seq.fold (fun acc x -> x - acc) 0

// Returns a sequence containing the solution to each part in order
let solve (input: string) =
    seq {
        let lines = input.Split(Environment.NewLine)
        let histories = lines |> Seq.map parseHistory
        let differences = histories |> Seq.map getDifferences

        yield differences |> Seq.map getNextValue |> Seq.sum
        yield differences |> Seq.map getPreviousValue |> Seq.sum
    }
