module Aoc2022.Tests

open Xunit
open Swensen.Unquote

module Common =
    open System.IO

    let load (name: string) (expected: int) =
        [| box (load name); box expected |]

    let loadInput (name: string) (expected: int) =
        [| box (loadInput name); box expected |]

open Common

module Day1 =
    let sample = "\
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"

    let ``part1 data`` =
        [| load sample 24000
           loadInput "day1" 72602 |]

    [<Theory; MemberData (nameof ``part1 data``)>]
    let part1 (input: string list) (expected: int) =
        let input = Day1.Parsing.parseInput input
        test <@ Day1.Domain.part1 input = expected @>

    let ``part2 data`` =
        [| load sample 45000
           loadInput "day1" 207410 |]

    [<Theory; MemberData (nameof ``part2 data``)>]
    let part2 (input: string list) (expected: int) =
        let input = Day1.Parsing.parseInput input
        test <@ Day1.Domain.part2 input = expected @>

module Day2 =
    let sample = "\
A Y
B X
C Z"

    let ``part1 data`` =
        [| load sample 15
           loadInput "day2" 13682 |]

    [<Theory; MemberData (nameof ``part1 data``)>]
    let part1 (input: string list) (expected: int) =
        let input = Day2.Part1.Parsing.parseInput input
        test <@ Day2.Part1.Domain.solve input = expected @>

    let ``part2 data`` =
        [| load sample 12
           loadInput "Day2" 12881 |]

    [<Theory; MemberData (nameof ``part2 data``)>]
    let part2 (input: string list) (expected: int) =
        let input = Day2.Part2.Parsing.parseInput input
        test <@ Day2.Part2.Domain.solve input = expected @>
