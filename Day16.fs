module Day16

open System
open System.Collections.Generic

type Direction =
    | Left
    | Right
    | Up
    | Down

let getNextPosition current direction =
    match direction with
    | Left -> (fst current, snd current - 1)
    | Right -> (fst current, snd current + 1)
    | Up -> (fst current - 1, snd current)
    | Down -> (fst current + 1, snd current)

let outOfBounds map position =
    fst position < 0
    || fst position >= Array2D.length1 map
    || snd position < 0
    || snd position >= Array2D.length2 map

let countEnergizedTiles map (initialPosition, initialDirection) =
    let history = Dictionary<_, _>()

    let rec trace position direction =
        let state = position, direction

        if outOfBounds map position || history.ContainsKey(state) then
            []
        else
            history.Add(state, ())

            match position ||> Array2D.get map, direction with
            | '-', Left
            | '-', Right
            | '|', Up
            | '|', Down
            | '.', _ -> position :: trace (getNextPosition position direction) direction
            | '/', Left -> position :: trace (getNextPosition position Down) Down
            | '/', Right -> position :: trace (getNextPosition position Up) Up
            | '/', Up -> position :: trace (getNextPosition position Right) Right
            | '/', Down -> position :: trace (getNextPosition position Left) Left
            | '\\', Left -> position :: trace (getNextPosition position Up) Up
            | '\\', Right -> position :: trace (getNextPosition position Down) Down
            | '\\', Up -> position :: trace (getNextPosition position Left) Left
            | '\\', Down -> position :: trace (getNextPosition position Right) Right
            | '-', _ ->
                let left = trace (getNextPosition position Left) Left
                let right = trace (getNextPosition position Right) Right
                position :: left @ right
            | '|', _ ->
                let up = trace (getNextPosition position Up) Up
                let down = trace (getNextPosition position Down) Down
                position :: up @ down
            | _ -> failwith "something wrong"

    trace initialPosition initialDirection |> Seq.distinct |> Seq.length

let parseMap lines =
    let rows = Array.length lines
    let columns = String.length lines[0]
    Array2D.init rows columns (fun r c -> lines[r].[c])

let getStartPositions map =
    let rows = Array2D.length1 map
    let cols = Array2D.length2 map

    List.init rows (fun row -> (row, 0), Right)
    @ List.init rows (fun row -> (row, cols - 1), Left)
    @ List.init cols (fun col -> (0, col), Down)
    @ List.init cols (fun col -> (rows - 1, col), Up)

let solve (input: string) =
    seq {
        let lines = input.Split(Environment.NewLine)
        let map = lines |> parseMap

        yield countEnergizedTiles map ((0, 0), Right)

        yield map |> getStartPositions |> Seq.map (countEnergizedTiles map) |> Seq.max
    }
