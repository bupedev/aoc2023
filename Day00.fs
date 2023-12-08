module Day00

open System

// Returns all lines of the input
let solve (input: string) =
    seq {
        let lines = input.Split(Environment.NewLine)
        yield lines
    }
