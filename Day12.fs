module Day12

open System
open System.Collections.Generic

type SpringState =
    | Operational
    | Damaged
    | Unknown

type Record =
    { SpringStates: seq<SpringState>
      ReportedDamageGroups: seq<int> }

let parseSpringState =
    function
    | '.' -> Operational
    | '#' -> Damaged
    | '?'
    | _ -> Unknown

let parseRecord (line: String) =
    let spaceSplit = line.Split(" ")
    let springStates = spaceSplit[0] |> Seq.map parseSpringState
    let damageGroupLengths = spaceSplit[1].Split(",") |> Seq.map int

    { SpringStates = springStates
      ReportedDamageGroups = damageGroupLengths }

let unfold (record: Record) =
    let springStates =
        Seq.init 5 (fun _ -> seq [ Unknown ] |> Seq.append record.SpringStates)
        |> Seq.concat
        |> Seq.take ((Seq.length record.SpringStates) * 5 + 4)

    let reportedDamageGroups =
        record.ReportedDamageGroups |> Seq.replicate 5 |> Seq.concat

    { SpringStates = springStates
      ReportedDamageGroups = reportedDamageGroups }

let countPermutations record =
    let cache = Dictionary<_, _>()
    let memoize func arguments =
        let success, value = cache.TryGetValue arguments
        if success then value else
            let value = func arguments
            cache.Add(arguments, value)
            value

    let rec permute (states, groups, currentGroup, needGap) =
        match states, groups, currentGroup, needGap with
            // We may be able to create a new damage group, or continue outside of one, so we account for both (this is where the permutation tree splits)...
            | Unknown :: remainingStates, nextGroup :: remainingGroups, 0, false ->
                (memoize permute (remainingStates, remainingGroups, nextGroup - 1, nextGroup = 1)) // Start damage group...
                + (memoize permute (remainingStates, groups, 0, false)) // Continue outside damage group...
            // We are outside of a damage group...
            | Unknown :: remainingStates, [], 0, false
            | Unknown :: remainingStates, _, 0, true
            | Operational :: remainingStates, _, 0, _ -> memoize permute (remainingStates, groups, 0, false)
            // We are at the start of a damaged group...
            | Damaged :: remainingStates, nextGroup :: remainingGroups, 0, false ->
                memoize permute (remainingStates, remainingGroups, nextGroup - 1, nextGroup = 1)
            // We are within a group (not at the end)...
            | Unknown :: remainingStates, _, currentGroup, false
            | Damaged :: remainingStates, _, currentGroup, false ->
                memoize permute (remainingStates, groups, currentGroup - 1, currentGroup = 1)
            // No states left to interpret with no groups left to satisfy, this indicates that we got to the end of a feasible permutation!
            | [], [], 0, _ -> 1L 
            // Infeasible case; we won't count this one...
            | _ -> 0L 

    permute (List.ofSeq record.SpringStates, List.ofSeq record.ReportedDamageGroups, 0, false)

let solve (input: string) =
    seq {
        let lines = input.Split(Environment.NewLine)
        
        let records = lines |> Seq.map parseRecord
        yield records |> Seq.map countPermutations |> Seq.sum
        
        let unfoldedRecords = records |> Seq.map unfold
        yield unfoldedRecords |> Seq.map countPermutations |> Seq.sum
    }
