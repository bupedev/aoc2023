module Day11

open System

// Represents the state of each tile in the universe
type Tile =
    | Space
    | Galaxy

// Parse a character as a tile 
let parseTile =
    function
    | '#' -> Galaxy
    | '.'
    | _ -> Space

// Parse all lines of the input as a universe
let parseUniverse (lines: string array) =
    let rows = Seq.length lines
    let cols = String.length lines[0]
    Array2D.init rows cols (fun row col -> parseTile lines[row].[col])

// Get the indices of the arrays of the universe (on either axis) that contain only space 
let getEmptyArrayIndices (indexCount: int) (getArray: int -> Tile array) =
    [ 0 .. indexCount - 1 ]
    |> Seq.map (fun index -> (index, getArray index))
    |> Seq.filter (fun (_, array) -> array |> Seq.forall (fun elem -> elem = Space))
    |> Seq.map fst
    |> Set.ofSeq

// Get the number of gaps that appear before the array index
let getOffset gapIndices arrayIndex =
    gapIndices
    |> Seq.filter (fun i -> i <= arrayIndex)
    |> Seq.length
    |> int64
    
// Get the location of all galaxies, accounting for the size of the "all-space" gaps
let getGalaxyLocations universe gapSize =    
    let horizontalGapIndices =
        getEmptyArrayIndices (Array2D.length1 universe) (fun i -> universe[i, *])

    let verticalGapIndices =
        getEmptyArrayIndices (Array2D.length2 universe) (fun i -> universe[*, i])

    [ for row in 0 .. (Array2D.length1 universe) - 1 do
         for col in 0 .. (Array2D.length2 universe) - 1 do
             if universe[row, col] = Galaxy then
                 let rowOffset = (gapSize - 1L) * getOffset horizontalGapIndices row
                 let colOffset = (gapSize - 1L) * getOffset verticalGapIndices col
                 yield (int64 row + rowOffset, int64 col + colOffset) ]
    
// Get all permutations of unordered pairs of elements in a list 
let getGetPermutations list = 
    [ for i in 0 .. (List.length list) - 2 do
         for j in i + 1 .. (List.length list) - 1 do
             yield (list.[i], list.[j]) ]
    
// Calculate the manhattan distance between a pair of two locations
let calculateManhattanDistances ((x1, y1), (x2, y2)) =
    abs (x2 - x1) + abs (y2 - y1)
        
// Returns a sequence containing the solution to each part in order
let solve (input: string) =
    seq {
        let universe = input.Split(Environment.NewLine) |> parseUniverse
        let galaxyLocations = getGalaxyLocations universe 1_000_000
        yield getGetPermutations galaxyLocations |> Seq.map calculateManhattanDistances |> Seq.sum
    }
