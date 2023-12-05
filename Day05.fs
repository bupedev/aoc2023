module Day05

open System
open System.Text.RegularExpressions

type Range = { Start: Int64; Length: Int64 }
type Transform = { Range: Range; Offset: Int64 }

// Calculate the exclusive upper limit of a range
let getLimit range = range.Start + range.Length

// Parse space delimited numbers into a list of numbers
let parseNumberList (str: String) =
    str.Trim().Split(" ", StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map Int64.Parse
    |> List.ofSeq

// Generate pairs of list elements
let rec pairElements list =
    match list with
    | first :: second :: tail -> (first, second) :: pairElements tail
    | _ -> []

// Parse seed string for a list of numbers
let parseSeedValueList (lines: seq<String>) =
    (Seq.head lines).Split(":").[1] |> parseNumberList

// Parse lines for seed string and treat pairs of numbers as the start and length of ranges
let parseSeedsAsSpannedRanges (lines: seq<String>) =
    parseSeedValueList lines
    |> pairElements
    |> Seq.map (fun (first, second) -> { Start = first; Length = second })

// Parse lines for seed string and treat numbers as single value ranges
let parseSeedsAsValueRanges (lines: seq<String>) =
    parseSeedValueList lines
    |> Seq.map (fun number -> { Start = number; Length = 1 })

// Splits a sequence based on some predicate (empty sub-sequences are removed)
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

// Create a transformation with no offset for some range
let standardTransform start length =
    { Range = { Start = start; Length = length }
      Offset = 0L }

// Create a transformation with some offset for some range
let offsetTransform start length offset =
    { Range = { Start = start; Length = length }
      Offset = offset }

// Fill gaps in the domain range with standard transforms
let fillGaps domain ranges =
    let rec fill start ranges =
        let attemptPreFill head tail start =
            let newRange =
                if head.Range.Start > start then
                    let length = head.Range.Start - start
                    [ standardTransform start length ]
                else
                    []

            newRange @ head :: fill (head.Range.Start + head.Range.Length) tail

        let attemptEndFill start =
            if start < (domain.Start + domain.Length) then
                let length = domain.Start + domain.Length - start
                [ standardTransform start length ]
            else
                []

        match ranges with
        | [] -> attemptEndFill start
        | head :: tail -> attemptPreFill head tail start

    fill domain.Start ranges

// Parse a block of transformation strings
let parseTransforms (lines: seq<string>) =
    let list =
        lines
        |> Seq.map parseNumberList
        |> Seq.map (fun numbers ->
            { Offset = numbers.[0] - numbers.[1]
              Range =
                { Start = numbers.[1]
                  Length = numbers.[2] } })
        |> Seq.sortBy (fun t -> t.Range.Start)
        |> List.ofSeq

    fillGaps { Start = 0L; Length = Int64.MaxValue } list

// Get the offset range of the transformation
let applyOffset transform =
    { Start = (transform.Range.Start + transform.Offset)
      Length = transform.Range.Length }

// Check if two ranges overlap
let isOverlapping a b =
    a.Start < (getLimit b) && b.Start < (getLimit a)

// Split a transform along its offset range against the downstream transform's range
let splitTransform baseTransform downstreamTransform =
    let offsetRange = applyOffset baseTransform
    let downstreamRange = downstreamTransform.Range

    let transformedStart = max offsetRange.Start downstreamRange.Start
    let transformedLimit = min (getLimit offsetRange) (getLimit downstreamRange)

    let start = transformedStart - baseTransform.Offset
    let length = transformedLimit - transformedStart
    let offset = baseTransform.Offset + downstreamTransform.Offset

    offsetTransform start length offset

// Split a set transforms against a downstream set of transforms
let splitTransformSets baseTransforms downstreamTransforms =
    baseTransforms
    |> List.collect (fun baseTransform ->
        downstreamTransforms
        |> List.choose (fun downstreamTransform ->
            if isOverlapping (applyOffset baseTransform) downstreamTransform.Range then
                Some(splitTransform baseTransform downstreamTransform)
            else
                None))

// Iteratively split transform sets across a list of transform sets
let splitAllTransformSets transformLists =
    match transformLists with
    | firstList :: restOfLists -> restOfLists |> List.fold splitTransformSets firstList
    | [] -> []

// Find the seed with the lowest location by splitting transforms and picking the lowest applied offset
let getLowestSeedLocation transformSets seedRanges =
    let seedTransforms =
        seedRanges
        |> Seq.sortBy (fun x -> x.Start)
        |> Seq.map (fun x -> { Range = x; Offset = 0 })
        |> List.ofSeq

    splitAllTransformSets transformSets
    |> splitTransformSets seedTransforms
    |> Seq.map (fun x -> x.Range.Start + x.Offset)
    |> Seq.min


// Returns a sequence containing the solution to each part in order.
let solve (input: string) =
    seq {
        let lines = input.Split(Environment.NewLine)

        let transformSets =
            split (fun (str: string) -> not (Regex.IsMatch(str, @"\d"))) (lines |> Seq.skip 1)
            |> Seq.map parseTransforms
            |> List.ofSeq

        yield parseSeedsAsValueRanges lines |> getLowestSeedLocation transformSets
        yield parseSeedsAsSpannedRanges lines |> getLowestSeedLocation transformSets
    }
