#load "Common.fs"
#r "nuget: FParsec"
#r "nuget: FSharpx.Extras"

open FParsec
open FSharpx.Collections

type Pos = { x: int; y: int } with

    static member (+) (a: Pos, v: Vec) : Pos =
        { x = a.x + v.x; y = a.y + v.y }

    static member (-) (a: Pos, b: Pos) : Vec =
        { x = a.x - b.x; y = a.y - b.y }

and Vec = { x: int; y: int }

type Sensor =
    { sensorPos: Pos
      beaconPos: Pos }

let pcoords =
    pipe2
        (skipString "x=" >>. pint32)
        (skipString ", y=" >>. pint32)
        (fun x y -> { x = x; y = y } : Pos)

let psensor =
    pipe2
        (skipString "Sensor at " >>. pcoords)
        (skipString ": closest beacon is at " >>. pcoords)
        (fun sensor beacon -> { sensorPos = sensor; beaconPos = beacon })

let pinput = sepEndBy psensor skipNewline

let parseInput input =
    match runParserOnString pinput () "" input with
    | Success(x, _, _) -> x
    | Failure(e, _, _) -> failwith e

let sample = parseInput "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
"

let length (v: Vec) =
    abs v.x + abs v.y

let sensorRange sensor =
    length (sensor.beaconPos - sensor.sensorPos)

type Range = { l: int; r: int }

let sensorRangeAtRow (row: int) (sensor: Sensor) =
    let range = sensorRange sensor
    let vdist = abs (row - sensor.sensorPos.y)
    if vdist > range then
        None
    else
        let hrad = range - vdist
        Some { l = sensor.sensorPos.x - hrad; r = sensor.sensorPos.x + hrad }

let rec addRange ranges range =
    match ranges with
    | [] -> [range]
    | range1 :: rest ->
        if range.r < range1.l - 1 then
            range :: ranges
        elif range1.r < range.l - 1 then
            range1 :: addRange rest range
        else
            addRange rest { l = min range.l range1.l; r = max range.r range1.r }

let sensorRangesAtRow (sensors: Sensor list) (row: int) =
    sensors
    |> List.choose (sensorRangeAtRow row)
    |> List.fold addRange []

sensorRangesAtRow sample 10

let rangeSize r = r.r - r.l + 1

let isInRange pos range = pos >= range.l && pos <= range.r

let isInRanges pos ranges = List.exists (isInRange pos) ranges

let part1 row sensors =
    let ranges = sensorRangesAtRow sensors row
    let rangeSizes = List.sumBy rangeSize ranges
    let beaconsInRange =
        sensors
        |> List.map (fun s -> s.beaconPos)
        |> List.filter (fun b -> b.y = row && isInRanges b.x ranges)
        |> List.distinct
        |> List.length
    rangeSizes - beaconsInRange

part1 10 sample
part1 2000000 (parseInput (loadInputAsOneString "day15"))

let isIncludedInRange needle haystack = needle.l >= haystack.l && needle.r <= haystack.r

let isIncludedInRanges needle haystack = List.exists (isIncludedInRange needle) haystack

let rec intersect ranges1 ranges2 =
    match ranges1, ranges2 with
    | [], _ | _, [] -> []
    | r1 :: rest1, r2 :: rest2 ->
        if r1.r < r2.l then
            intersect rest1 ranges2
        elif r2.r < r1.l then
            intersect ranges1 rest2
        elif r1.r = r2.r then
            { l = max r1.l r2.l; r = r1.r } :: intersect rest1 rest2
        elif r1.r > r2.r then
            { l = max r1.l r2.l; r = r2.r } :: intersect ranges1 rest2
        else
            { l = max r1.l r2.l; r = r1.r } :: intersect rest1 ranges2

let part2 maxCoord sensors =
    let searchRanges = [ { l = 0; r = maxCoord } ]
    {0..maxCoord}
    |> Seq.map (fun row ->
        let ranges = sensorRangesAtRow sensors row
        match intersect ranges searchRanges with
        | r when r = searchRanges -> None
        | [ r1; r2 ] when r1.l = 0 && r2.r = maxCoord && r1.r + 2 = r2.l -> Some (r1.r + 1)
        | r -> failwith $"Several beacons found at y={row}; ranges = {r}")
    |> Seq.tryFindWithIndex Option.isSome
    |> function
        | Some (y, Some x) -> x, y, int64 x * 4000000L + int64 y
        | _ -> failwith "No beacon found"

part2 20 sample
part2 4000000 (parseInput (loadInputAsOneString "day15"))