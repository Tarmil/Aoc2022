#load "Common.fs"
#r "nuget: FParsec"

open FParsec

type Range = Range of int * int

let contains (Range(needle1, needle2)) (Range(haystack1, haystack2)) =
    needle1 >= haystack1 && needle2 <= haystack2

contains (Range(1, 3)) (Range(0, 5))
contains (Range(1, 3)) (Range(2, 5))

let oneContainsTheOther range1 range2 =
    (range1 |> contains range2) || (range2 |> contains range1)

oneContainsTheOther (Range(1, 3)) (Range(0, 5))
oneContainsTheOther (Range(0, 5)) (Range(1, 3))

let prange = pint32 .>> pchar '-' .>>. pint32 |>> Range
let pline = prange .>> pchar ',' .>>. prange

let parseLine s =
    match runParserOnString pline () "" s with
    | Success(x, _, _) -> x
    | Failure(e, _, _) -> failwith e

parseLine "2-4,6-8"

let part1 input =
    List.map parseLine input
    |> List.filter (fun (r1, r2) -> oneContainsTheOther r1 r2)
    |> List.length

part1 <| load "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"

part1 <| loadInput "day4"

let areSeparate (Range(x1, x2)) (Range(y1, y2)) =
    x2 < y1 || y2 < x1

areSeparate (Range(1, 5)) (Range(3, 8))
areSeparate (Range(1, 3)) (Range(5, 8))
areSeparate (Range(5, 8)) (Range(1, 3))

let part2 input =
    List.map parseLine input
    |> List.filter (fun (r1, r2) -> not (areSeparate r1 r2))
    |> List.length

part2 <| load "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"

part2 <| loadInput "day4"
