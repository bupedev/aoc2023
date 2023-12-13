module Day13

open System

type Axis =
    | Horizontal
    | Vertical

type Reflection = { Axis: Axis; Index: int }

type Pattern =
    { Vertical: int list
      Horizontal: int list }

let split predicate sequence =
    let groupByPredicate (acc, current) item =
        if predicate item then
            (List.rev current :: acc, [])
        else
            (acc, item :: current)

    let resolveGrouping (acc, current) =
        if current <> [] then List.rev current :: acc else acc

    sequence
    |> Seq.fold groupByPredicate ([], [])
    |> resolveGrouping
    |> Seq.rev
    |> Seq.filter (fun group -> group <> [])
    |> Seq.map Array.ofList

let createMatrix (lines: string array) =
    Array2D.init (Array.length lines) (String.length lines[0]) (fun row col -> lines[row][col])

let getSequenceValue index element =
    match element with
    | '#' -> (1 <<< index)
    | _ -> 0

let createSequenceInteger (elements: char seq) =
    elements |> Seq.mapi getSequenceValue |> Seq.sum

let parsePattern (lines: string array) =
    let matrix = createMatrix lines

    let horizontalSequences =
        Seq.init (Array2D.length1 matrix) (fun index -> matrix[index, *])
        |> Seq.map createSequenceInteger
        |> List.ofSeq

    let verticalSequences =
        Seq.init (Array2D.length2 matrix) (fun index -> matrix[*, index])
        |> Seq.map createSequenceInteger
        |> List.ofSeq

    { Vertical = verticalSequences
      Horizontal = horizontalSequences }

let rec pairElements list =
    match list with
    | first :: second :: tail -> (first, second) :: pairElements (second :: tail)
    | _ -> []

let isTrueReflection (values: int list) (reflectionIndex: int) =
    Seq.zip (Seq.rev values[..reflectionIndex]) values[(reflectionIndex + 1) ..]
    |> Seq.forall (fun (left, right) -> left = right)

let getTrueReflectionIndices (values: int list) =
    values
    |> pairElements
    |> Seq.mapi (fun index pair -> (index, pair))
    |> Seq.filter (fun (_, pair) -> (fst pair) = (snd pair))
    |> Seq.map fst
    |> Seq.filter (isTrueReflection values)

let getPatternReflections pattern =
    let horizontal =
        getTrueReflectionIndices pattern.Horizontal
        |> Seq.map (fun index -> { Axis = Horizontal; Index = index })

    let vertical =
        getTrueReflectionIndices pattern.Vertical
        |> Seq.map (fun index -> { Axis = Vertical; Index = index })

    [ horizontal; vertical ] |> Seq.concat |> Set.ofSeq

let getReflectionValue reflection =
    match reflection.Axis with
    | Horizontal -> 100 * (reflection.Index + 1)
    | Vertical -> (reflection.Index + 1)

let getAlternativePattern pattern indices =
    let alternativeHorizontal =
        pattern.Horizontal
        |> List.mapi (fun index value ->
            if index = fst indices then
                value ^^^ (1 <<< snd indices)
            else
                value)

    let alternativeVertical =
        pattern.Vertical
        |> List.mapi (fun index value ->
            if index = snd indices then
                value ^^^ (1 <<< fst indices)
            else
                value)

    { Horizontal = alternativeHorizontal
      Vertical = alternativeVertical }

let getAlternativePatternReflection pattern =
    let patternIndices =
        [ for row in 0 .. (Seq.length pattern.Horizontal) - 1 do
              for col in 0 .. (Seq.length pattern.Vertical) - 1 do
                  yield (row, col) ]

    let originalReflection = getPatternReflections pattern |> Seq.head

    patternIndices
    |> Seq.map (getAlternativePattern pattern)
    |> Seq.map getPatternReflections
    |> Seq.concat
    |> Set.ofSeq
    |> Set.remove originalReflection
    |> Seq.head

let solve (input: string) =
    seq {
        let lines = input.Split(Environment.NewLine)
        let split = split String.IsNullOrEmpty lines
        let patterns = Seq.map parsePattern split |> Array.ofSeq

        yield
          patterns
          |> Seq.map getPatternReflections
          |> Seq.concat
          |> Seq.map getReflectionValue
          |> Seq.sum

        yield
          patterns
          |> Seq.map getAlternativePatternReflection
          |> Seq.map getReflectionValue
          |> Seq.sum   
    } 