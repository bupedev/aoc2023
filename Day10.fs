module Day10

open System

type Direction =
    | Up
    | Right
    | Down
    | Left

type Tile =
    | Vertical
    | Horizontal
    | TopRight
    | TopLeft
    | BottomRight
    | BottomLeft
    | Start
    | Ground

let getTile character =
    match character with
    | '|' -> Vertical
    | '-' -> Horizontal
    | '7' -> TopRight
    | 'F' -> TopLeft
    | 'J' -> BottomRight
    | 'L' -> BottomLeft
    | 'S' -> Start
    | _ -> Ground

let parseMap lines =
    let rows = Array.length lines
    let cols = String.length lines[0]
    Array2D.init rows cols (fun row col -> getTile lines[row].[col])

let outOfBounds map location =
    let row, col = location
    row < 0 || col < 0 || row >= Array2D.length1 map || col >= Array2D.length2 map

// Get output direction of a tile given an input location if it is feasible.
let getDirection direction tile =
    match (direction, tile) with
    | Up, Vertical -> Some(direction)
    | Up, TopRight -> Some(Left)
    | Up, TopLeft -> Some(Right)
    | Right, Horizontal -> Some(direction)
    | Right, BottomRight -> Some(Up)
    | Right, TopRight -> Some(Down)
    | Down, Vertical -> Some(direction)
    | Down, BottomLeft -> Some(Right)
    | Down, BottomRight -> Some(Left)
    | Left, Horizontal -> Some(direction)
    | Left, BottomLeft -> Some(Up)
    | Left, TopLeft -> Some(Down)
    | _ -> None

// Get the location in some direction from another location
let getNextLocation (location: int * int) (direction: Direction) =
    let row, column = location

    match direction with
    | Direction.Up -> (row - 1, column)
    | Direction.Right -> (row, column + 1)
    | Direction.Down -> (row + 1, column)
    | Direction.Left -> (row, column - 1)

let getPathLoop (map: Tile array2d) (startLocation: int * int) (direction: Direction) =
    let rec recursePath location direction accPath =
        if outOfBounds map location then
            accPath
        else
            let tile = location ||> Array2D.get map

            if tile = Start then
                location :: accPath
            else
                match getDirection direction tile with
                | Some(nextDirection) ->
                    let nextLocation = getNextLocation location nextDirection
                    recursePath nextLocation nextDirection (location :: accPath)
                | None -> accPath

    let startOffset = getNextLocation startLocation direction
    recursePath startOffset direction [] |> List.rev

let getAllLocations (map: Tile array2d) =
    [ for i in 0 .. (Array2D.length1 map) - 1 do
          for j in 0 .. (Array2D.length2 map) - 1 do
              yield (i, j) ]

let findStart (map: Tile array2d) =
    getAllLocations map
    |> Seq.filter (fun location -> (location ||> Array2D.get map) = Start)
    |> Seq.head

let getStartTile directions =
    match Seq.toArray directions with
    | [| Left; Down |]
    | [| Down; Left |] -> TopRight
    | [| Right; Down |]
    | [| Down; Right |] -> TopLeft
    | [| Left; Up |]
    | [| Up; Left |] -> BottomRight
    | [| Right; Up |]
    | [| Up; Right |] -> BottomLeft
    | _ -> Ground

let replaceStartTile map (row, col) directions =
    let replacement = getStartTile directions
    Array2D.set map row col replacement

let countInsideTiles (map: Tile array2d) (pathTileSet: Set<int * int>) (rowIndex: int) =
    let row = map[rowIndex, *]

    let rec scanline cellIndex lastCorner inside count =
        if cellIndex < Array.length row then
            let cell = row[cellIndex]
            let onPath = pathTileSet.Contains((rowIndex, cellIndex))

            let nextLastCorner =
                if onPath then
                    match cell with
                    | TopLeft
                    | BottomLeft -> cell
                    | _ -> lastCorner
                else
                    lastCorner

            let flip =
                if onPath then
                    match (cell, lastCorner) with
                    | BottomRight, TopLeft
                    | TopRight, BottomLeft
                    | Vertical, _ -> true
                    | _ -> false
                else
                    false

            let increment = if not onPath && inside then 1 else 0

            scanline (cellIndex + 1) nextLastCorner (inside <> flip) (count + increment)
        else
            count

    scanline 0 Ground false 0

let countTilesInsidePath map pathTileSet =
    let rec countMapByRow (rowIndex: int) (count: int) =
        if rowIndex < Array2D.length1 map then
            let inside = countInsideTiles map pathTileSet rowIndex
            countMapByRow (rowIndex + 1) (count + inside)
        else
            count

    countMapByRow 0 0

let solve (input: string) =
    seq {
        let map = input.Split(Environment.NewLine) |> parseMap
        let startLocation = findStart map

        let loopPaths =
            [| Direction.Up; Direction.Right; Direction.Down; Direction.Left |]
            |> Seq.map (fun direction -> (direction, getPathLoop map startLocation direction))
            |> Seq.filter (fun (_, path) -> Seq.length path > 0 && Seq.last path = startLocation)

        let pathTileSet = Seq.head loopPaths |> snd |> Set.ofSeq

        yield (Seq.length pathTileSet) / 2

        replaceStartTile map startLocation (loopPaths |> Seq.map fst)

        yield countTilesInsidePath map pathTileSet
    }
