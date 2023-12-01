module Day01

open System

type CharPosition = { Character: char; Index: int }

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
    |> Seq.mapi (fun i x -> { Character = x; Index = i })
    |> Seq.filter (fun charPosition -> Char.IsDigit charPosition.Character)
 
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
    |> Seq.collect (
        fun word ->
            findAllIndices str word
            |> Seq.map (
                fun index -> { Character = wordDigitMap[word]; Index = index}
            )
    )

// Combine the first and last character and convert to an integer
let combineFirstAndLastDigit digitPositions =
    if Seq.length digitPositions = 0
    then 0
    else  
        digitPositions
        |> Seq.sortBy (fun x -> x.Index)
        |> Seq.map (fun x -> x.Character)
        |> (fun s -> [| Seq.head s; Seq.last s |] |> String |> int)

// Returns a sequence containing the solution to each part in order.
let solve (input: string) =
    seq {
        let lines = input.Split "\n" |> Array.ofSeq

        let pureDigitsWithPositions = lines |> Seq.map getPureDigitPositions
        
        yield pureDigitsWithPositions
            |> Seq.map combineFirstAndLastDigit
            |> Seq.sum

        let wordDigitsWithPositions = lines |> Seq.map getWordDigitPositions
        
        yield Seq.zip pureDigitsWithPositions wordDigitsWithPositions
            |> Seq.map (fun (s1, s2) -> Seq.concat [s1; s2])
            |> Seq.map combineFirstAndLastDigit
            |> Seq.sum
    }