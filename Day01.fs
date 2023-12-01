module Day01

open System

// A map of all word digits to pure digits
let wordDigitMap = Map [
    ("one", '1');
    ("two", '2');
    ("three", '3');
    ("four", '4');
    ("five", '5');
    ("six", '6');
    ("seven", '7');
    ("eight", '8');
    ("nine", '9');
]

// Get all pure digits in a string with their positions
let getPureDigitPositions (str: string) =
    str
    |> Seq.mapi (fun i x -> (x, i))
    |> Seq.filter (fun (x, _) -> Char.IsDigit x)

// Find the indices of all instances of a word in a string
let findAllIndices (str: string) (word: string) =
    let rec findFromIndex currentIndex =
        seq {
            if currentIndex < 0 || currentIndex >= str.Length then
                yield! Seq.empty
            else
                let foundIndex = str.IndexOf(word, currentIndex)

                if foundIndex >= 0 then
                    yield foundIndex
                    yield! findFromIndex (foundIndex + word.Length)
        }

    findFromIndex 0

// Get all word digits in a string with their positions
let getWordDigitPositions str =
    wordDigitMap.Keys
    |> Seq.collect (fun word -> findAllIndices str word |> Seq.map (fun index -> (wordDigitMap[word], index)))

// Combine the first and last character and convert to an integer
let combineFirstAndLastDigit digitPositions =
    digitPositions
    |> Seq.sortBy snd
    |> Seq.map fst
    |> (fun s -> [| Seq.head s; Seq.last s |] |> String |> int)

// ENTRY POINT
let solve (input: string) =
    let lines = input.Split "\n" |> Array.ofSeq

    let pureDigitsWithPositions = lines |> Seq.map getPureDigitPositions
    
    let badCalibrationTotal =
        pureDigitsWithPositions
        |> Seq.map combineFirstAndLastDigit
        |> Seq.sum

    printf $"Part 1: %d{badCalibrationTotal}\n"

    let wordDigitsWithPositions = lines |> Seq.map getWordDigitPositions
    
    let goodCalibrationTotal =
        Seq.zip pureDigitsWithPositions wordDigitsWithPositions
        |> Seq.map (fun (s1, s2) -> Seq.concat [s1; s2])
        |> Seq.map combineFirstAndLastDigit
        |> Seq.sum

    printf $"Part 2: %d{goodCalibrationTotal}\n"
