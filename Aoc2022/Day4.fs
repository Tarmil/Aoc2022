module Aoc2022.Day4

open FSharpx

module Common =

    module Domain =

        type Range = Range of int * int

    module Parsing =
        open FParsec
        open Domain

        let prange = pint32 .>> pchar '-' .>>. pint32 |>> Range

        let pline = prange .>> pchar ',' .>>. prange

        let parseLine s =
            match runParserOnString pline () "line" s with
            | Success (x, _, _) -> Result.Ok x
            | Failure (e, _, _) -> Result.Error e

        let parseInput (input: string list) : Result<(Range * Range) list, string> =
            input
            |> List.map parseLine
            |> Result.sequence

module Part1 =
    open Common.Domain

    module Domain =

        let contains (Range(needle1, needle2)) (Range(haystack1, haystack2)) =
            needle1 >= haystack1 && needle2 <= haystack2

        let oneContainsTheOther range1 range2 =
            (range1 |> contains range2) || (range2 |> contains range1)

        let solve (input: (Range * Range) list) =
            input
            |> List.filter (fun (r1, r2) -> oneContainsTheOther r1 r2)
            |> List.length

    module Parsing =

        let parseInput input = Common.Parsing.parseInput input

module Part2 =
    open Common.Domain

    module Domain =

        let areSeparate (Range(x1, x2)) (Range(y1, y2)) =
            x2 < y1 || y2 < x1

        let solve (input: (Range * Range) list) =
            input
            |> List.filter (fun (r1, r2) -> not (areSeparate r1 r2))
            |> List.length

    module Parsing =

        let parseInput input = Common.Parsing.parseInput input
