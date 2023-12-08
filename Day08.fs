module Day08

open System

// Node data structure for the tree
type Node =
    { Name: string
      Left: string
      Right: string }

// Parse a node data structure from a line of the input
let parseNode (line: string) =
    let punctuation = [| '='; ' '; '('; ','; ')' |]
    let names = line.Split(punctuation, StringSplitOptions.RemoveEmptyEntries)

    { Name = names[0]
      Left = names[1]
      Right = names[2] }

// Recursively count the steps required to reach a node ending with "Z"
let rec countSteps (node: string) (step: int64) (directions: string) (nodeMap: Map<string, Node>) =
    if node.EndsWith("Z") then
        step
    else
        let direction = directions[int (step % int64 directions.Length)]

        let child =
            match direction with
            | 'L' -> nodeMap[node].Left
            | 'R' -> nodeMap[node].Right
            | _ -> failwith "something wrong"

        countSteps child (step + 1L) directions nodeMap

// Find the lowest common multiple (LCM) of a sequence of numbers
let lcm (seq: seq<int64>) =
    let rec gcd a b = if b = 0L then a else gcd b (a % b)
    seq |> Seq.reduce (fun a b -> (a * b) / gcd a b)

// Returns all lines of the input
let solve (input: string) =
    seq {
        let lines = input.Split(Environment.NewLine)

        let directions = lines[0]

        let nodeMap =
            lines[2..]
            |> Seq.map parseNode
            |> Seq.map (fun node -> (node.Name, node))
            |> Map.ofSeq

        yield countSteps "AAA" 0 directions nodeMap

        let startNodes = nodeMap.Keys |> Seq.filter (fun node -> node.EndsWith("A"))

        yield
            startNodes
            |> Seq.map (fun start -> countSteps start 0 directions nodeMap)
            |> lcm
    }
