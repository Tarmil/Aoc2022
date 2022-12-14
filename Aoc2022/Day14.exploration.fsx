#load "Common.fs"
#r "nuget: FParsec"

open FParsec

let pcoords = (pint32 .>> skipChar ',') .>>. pint32
let pline = sepBy pcoords (skipString " -> ")
let pinput = sepEndBy pline skipNewline
let parseInput input =
    match runParserOnString pinput () "" input with
    | Success(x, _, _) -> x
    | Failure(e, _, _) -> failwith e

let sample = parseInput "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
"

let limits input =
    let start = (500, 0)
    ((start, start), input)
    ||> List.fold (List.fold (fun ((xMin, yMin), (xMax, yMax)) (x, y) ->
        (min x xMin, min y yMin), (max x xMax, max y yMax)))

limits sample

type Tile =
    | Air
    | Rock
    | Sand

let drawLine (grid: Tile[,]) (x1, y1) (x2, y2) =
    if x1 = x2 then
        for y in y1 .. (sign (y2 - y1)) .. y2 do
            grid[x1, y] <- Rock
    elif y1 = y2 then
        for x in x1 .. (sign (x2 - x1)) .. x2 do
            grid[x, y1] <- Rock
    else
        failwith $"Incorrect line: {(x1, y1)} -> {(x2, y2)}"

let initGrid input =
    let (xMin, yMin), (xMax, yMax) = limits input
    let grid = Array2D.createBased xMin yMin (xMax - xMin + 1) (yMax - yMin + 1) Air
    for s in input do
        for (start, finish) in List.pairwise s do
            drawLine grid start finish
    grid

let printGrid (grid: Tile[,]) =
    for y in grid.GetLowerBound(1) .. grid.GetUpperBound(1) do
        for x in grid.GetLowerBound(0) .. grid.GetUpperBound(0) do
            match grid[x, y] with
            | Air -> '.'
            | Rock -> '#'
            | Sand -> 'O'
            |> printf "%c"
        printfn ""

initGrid sample
|> printGrid

type MoveResult =
    | MoveTo of int * int
    | MoveOffGrid
    | Stop

let moveSand (grid: Tile[,]) (x, y) =
    if y >= grid.GetUpperBound(1) then
        MoveOffGrid
    elif grid[x, y + 1] = Air then
        MoveTo (x, y + 1)
    elif x <= grid.GetLowerBound(0) then
        MoveOffGrid
    elif grid[x - 1, y + 1] = Air then
        MoveTo (x - 1, y + 1)
    elif x >= grid.GetUpperBound(0) then
        MoveOffGrid
    elif grid[x + 1, y + 1] = Air then
        MoveTo (x + 1, y + 1)
    else
        Stop

let grid = initGrid sample
MoveTo (500, 0) |> Seq.unfold (function
    | MoveTo (x, y) ->
        let dest = moveSand grid (x, y)
        Some (dest, dest)
    | _ -> None)
|> List.ofSeq

let rec dropSand grid (x, y) =
    match moveSand grid (x, y) with
    | MoveTo (x, y) -> dropSand grid (x, y)
    | MoveOffGrid -> false
    | Stop ->
        grid[x, y] <- Sand
        true

while dropSand grid (500, 0) do printGrid grid; printfn ""

let part1 input =
    let grid = initGrid input
    Seq.initInfinite (fun _ -> (500, 0))
    |> Seq.takeWhile (dropSand grid)
    |> Seq.length

part1 sample
part1 (loadInputAsOneString "day14" |> parseInput)

let initGrid2 input =
    let (xMin, yMin), (xMax, yMax) = limits input
    let stackSize = yMax + 2 // it's both the height and half the width of the stack
    let floor = [(xMin - stackSize - 1, stackSize); (xMax + stackSize + 1, stackSize)]
    initGrid (floor :: input)

initGrid2 sample
|> printGrid

let part2 input =
    let grid = initGrid2 input
    Seq.initInfinite (fun _ -> (500, 0))
    |> Seq.takeWhile (fun (x, y) -> grid[x, y] = Air && dropSand grid (x, y))
    |> Seq.length

part2 sample
part2 (loadInputAsOneString "day14" |> parseInput)
