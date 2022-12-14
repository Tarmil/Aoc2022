module Aoc2022.Day14

open FSharpx

module Common =

    module Domain =

        type Tile = Air | Rock | Sand

        type Grid = Tile[,]

        type Coords = int * int

        type Wall = Coords * Coords

        type WallSet = Coords list

        let dropPoint: Coords = (500, 0)

        let limits (walls: WallSet list) =
            ((dropPoint, dropPoint), walls)
            ||> List.fold (List.fold (fun ((xMin, yMin), (xMax, yMax)) (x, y) ->
                (min x xMin, min y yMin), (max x xMax, max y yMax)))

        let drawWall (grid: Grid) (((x1, y1), (x2, y2)): Wall) =
            if x1 = x2 then
                for y in y1 .. (sign (y2 - y1)) .. y2 do
                    grid[x1, y] <- Rock
                Ok ()
            elif y1 = y2 then
                for x in x1 .. (sign (x2 - x1)) .. x2 do
                    grid[x, y1] <- Rock
                Ok ()
            else
                Error $"Incorrect wall: {x1},{y1} -> {x2},{y2}"

        let drawWallSet (grid: Grid) (wallSet: WallSet) =
            wallSet
            |> List.pairwise
            |> List.map (drawWall grid)
            |> Result.sequence

        let initGrid wallSet =
            let (xMin, yMin), (xMax, yMax) = limits wallSet
            let xLength = xMax - xMin + 1
            let yLength = yMax - yMin + 1
            let grid = Array2D.createBased xMin yMin xLength yLength Air
            wallSet
            |> List.map (drawWallSet grid)
            |> Result.sequence
            |> Result.map (fun _ -> grid)

        type DropStep =
            | DropTo of Coords
            | DropOffGrid
            | Stop

        let dropStep (grid: Grid) ((x, y): Coords) : DropStep =
            if y >= grid.GetUpperBound(1) then
                DropOffGrid
            elif grid[x, y + 1] = Air then
                DropTo (x, y + 1)
            elif x <= grid.GetLowerBound(0) then
                DropOffGrid
            elif grid[x - 1, y + 1] = Air then
                DropTo (x - 1, y + 1)
            elif x >= grid.GetUpperBound(0) then
                DropOffGrid
            elif grid[x + 1, y + 1] = Air then
                DropTo (x + 1, y + 1)
            else
                Stop

        /// Return true if the sand has stopped, false if it went off grid.
        let rec dropSand (grid: Grid) (x, y as coords: Coords) : bool =
            match dropStep grid coords with
            | DropTo coords -> dropSand grid coords
            | DropOffGrid -> false
            | Stop ->
                grid[x, y] <- Sand
                true

        let dropAllSand (grid: Grid) : int =
            Seq.initInfinite (fun _ -> dropPoint)
            |> Seq.takeWhile (fun (x, y as coords) ->
                grid[x, y] = Air && dropSand grid coords)
            |> Seq.length

    module Parsing =
        open FParsec

        let pcoords = (pint32 .>> skipChar ',') .>>. pint32

        let pwallSet = sepBy pcoords (skipString " -> ")

        let pinput = sepEndBy pwallSet skipNewline

        let parseInput input =
            match runParserOnString pinput () "input" input with
            | Success(x, _, _) -> Result.Ok x
            | Failure(e, _, _) -> Result.Error e

module Part1 =

    module Domain =
        open Common.Domain

        let solve input =
            initGrid input
            |> Result.map dropAllSand

    let solve input =
        Common.Parsing.parseInput input
        |> Result.bind Domain.solve

module Part2 =

    module Domain =
        open Common.Domain

        let floor (input: WallSet list) =
            let (xMin, yMin), (xMax, yMax) = limits input
            let stackSize = (yMax - yMin) + 2 // it's both the height and half the width of the stack
            [(xMin - stackSize - 1, stackSize); (xMax + stackSize + 1, stackSize)]

        let solve input =
            initGrid (floor input :: input)
            |> Result.map dropAllSand

    let solve input =
        Common.Parsing.parseInput input
        |> Result.bind Domain.solve
