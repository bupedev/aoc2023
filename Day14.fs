module Day14

open System

type Direction =
    | North
    | East
    | South
    | West

let parsePattern lines =
    let rows = Array.length lines
    let cols = String.length lines[0]

    Array2D.init rows cols (fun row col -> lines[row].[col])

let rollForward (tiles: char array) =
    let rec loop index sequences =
        if index >= Seq.length tiles then
            sequences
        elif tiles[index] = '#' then
            loop (index + 1) ([ '#' ] :: sequences)
        else
            let rollable =
                tiles
                |> Seq.skip index
                |> Seq.takeWhile (fun tile -> tile <> '#')
                |> Seq.sortDescending
                |> List.ofSeq

            loop (index + (List.length rollable)) (rollable :: sequences)

    loop 0 [] |> Seq.concat |> Seq.rev |> Array.ofSeq

let rollBackward (tiles: char array) =
    let rec loop index sequences =
        if index >= Seq.length tiles then
            sequences
        elif tiles[index] = '#' then
            loop (index + 1) ([ '#' ] :: sequences)
        else
            let rollable =
                tiles
                |> Seq.skip index
                |> Seq.takeWhile (fun tile -> tile <> '#')
                |> Seq.sort
                |> List.ofSeq

            loop (index + (List.length rollable)) (rollable :: sequences)

    loop 0 [] |> Seq.concat |> Seq.rev |> Array.ofSeq

let rebuildFromColumns (columns: char array array) =
    Array2D.init (Seq.length columns[0]) (Seq.length columns) (fun row col -> columns[col][row])

let rebuildFromRows (rows: char array array) =
    Array2D.init (Seq.length rows) (Seq.length rows[0]) (fun row col -> rows[row][col])

let getColumns pattern =
    [ 0 .. (Array2D.length2 pattern) - 1 ]
    |> Seq.map (fun index -> pattern[*, index])
    |> Array.ofSeq

let getRows pattern =
    [ 0 .. (Array2D.length1 pattern) - 1 ]
    |> Seq.map (fun index -> pattern[index, *])
    |> Array.ofSeq

let roll (direction: Direction) (pattern: char array2d) =
    match direction with
    | North -> pattern |> getColumns |> Array.map rollBackward |> rebuildFromColumns
    | East -> pattern |> getRows |> Array.map rollForward |> rebuildFromRows
    | South -> pattern |> getColumns |> Array.map rollForward |> rebuildFromColumns
    | West -> pattern |> getRows |> Array.map rollBackward |> rebuildFromRows

let cycle pattern =
    pattern |> roll North |> roll West |> roll South |> roll East

let calculateLoad (pattern: char array2d) =
    Seq.init (Array2D.length1 pattern) (fun index -> ((Array2D.length1 pattern) - index, pattern[index, *]))
    |> Seq.fold
        (fun acc (weight, tiles) ->
            acc
            + weight * (tiles |> Seq.filter (fun tile -> tile = 'O') |> Seq.length))
        0

let calculateHash (pattern: char array2d) =
    let rows = pattern.GetLength(0)
    let cols = pattern.GetLength(1)
    let hash = HashCode()

    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            hash.Add(pattern[i, j])

    hash.ToHashCode()

let test pattern =
    let rec it index pattern loads =
        if index >= 1000
        then loads
        else
            let next = cycle pattern
            let hash = calculateHash next
            let load = calculateLoad next
            it (index + 1) next ((hash,load) :: loads)
            
    it 0 pattern [(calculateHash pattern, calculateLoad pattern)] |> Seq.rev
    
let predictNthElement n (sequence: (int * int) seq) =
    let rec findCycle seq map currentIndex =
        match Seq.tryHead seq with
        | None -> None  // End of sequence
        | Some (hash, _) ->
            match Map.tryFind hash map with
            | Some cycleStart -> Some (cycleStart, currentIndex - cycleStart)
            | None ->
                let updatedMap = Map.add hash currentIndex map
                findCycle (Seq.tail seq) updatedMap (currentIndex + 1)

    let map = Map.empty
    match findCycle sequence map 0 with
    | None -> failwith "No cycle detected in the sequence"
    | Some (cycleStart, cycleLength) ->
        let cyclePosition = (n - cycleStart) % cycleLength
        sequence |> Seq.skip cycleStart |> Seq.item cyclePosition |> snd

let solve (input: string) =
    seq {
        let lines = input.Split(Environment.NewLine)
        let pattern = parsePattern lines
        yield pattern |> test |> List.ofSeq
    }
