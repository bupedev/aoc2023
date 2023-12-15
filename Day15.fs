module Day15

open System

type Instruction =
    { Label: string
      Box: int
      Operation: char
      Lens: int }

let hash (str: string) =
    str |> Seq.map int |> Seq.fold (fun acc value -> (17 * (acc + value)) % 256) 0

let toString chars = chars |> Array.ofSeq |> String

let parseInstruction (str: string) =
    let label = str |> Seq.takeWhile (fun c -> c <> '=' && c <> '-') |> toString
    
    let box = hash label
    
    let opPosition = String.length label
    let operation = str[opPosition]

    let lens = if operation = '=' then str |> Seq.skip (1 + opPosition) |> toString |> int else 0

    { Label = label
      Box = box
      Operation = operation
      Lens = lens }

let rec replaceAtIndex index newValue list =
    match index, list with
    | _, [] -> []
    | 0, _ :: tail -> newValue :: tail
    | i, head :: tail -> head :: replaceAtIndex (i - 1) newValue tail

let arrangeBoxes instructions =
    let boxes = Array.init 256 (fun _ -> [])

    let execute instruction =
        let boxIndex = instruction.Box
        let box = boxes[instruction.Box]
        let label = instruction.Label
        let operation = instruction.Operation
        let lens = instruction.Lens
        let record = label, lens

        match operation with
        | '-' -> boxes[boxIndex] <- box |> List.filter (fun record -> fst record <> label)
        | '=' ->
            match box |> List.tryFindIndex (fun record -> fst record = label) with
            | Some(existingIndex) -> boxes[boxIndex] <- box |> replaceAtIndex existingIndex record
            | None -> boxes[boxIndex] <- record :: box
        | _ -> failwith "something wrong"

    instructions |> Seq.iter execute
    boxes

let calculateFocusingPower boxIndex box =
    box
    |> Seq.rev
    |> Seq.mapi (fun lensIndex record -> (lensIndex, snd record))
    |> Seq.fold (fun acc (lensIndex, lens) -> acc + (boxIndex + 1) * (lensIndex + 1) * lens) 0

// Returns all lines of the input
let solve (input: string) =
    seq {
        let lines = input.Split(Environment.NewLine)
        let instructionStrings = lines[0].Split(",")
        yield instructionStrings |> Seq.map hash |> Seq.sum

        let instructions = instructionStrings |> Seq.map parseInstruction
        let boxes = arrangeBoxes instructions
        yield boxes |> Seq.mapi calculateFocusingPower |> Seq.sum
    }
