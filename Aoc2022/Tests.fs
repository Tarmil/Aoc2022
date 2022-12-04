module Aoc2022.Tests

open Xunit
open Swensen.Unquote

module Common =

    let load (name: string) (expected: int) =
        [| box (load name); box expected |]

    let loadInput (name: string) (expected: int) =
        [| box (loadInput name); box expected |]

    let private getOk = function
        | Ok x -> x
        | Error e -> failwith e

    let checkOk expr =
        trap <@ getOk %expr @>

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
        let input = checkOk <@ Day1.Part1.Parsing.parseInput input @>
        test <@ Day1.Part1.Domain.solve input = expected @>

    let ``part2 data`` =
        [| load sample 45000
           loadInput "day1" 207410 |]

    [<Theory; MemberData (nameof ``part2 data``)>]
    let part2 (input: string list) (expected: int) =
        let input = checkOk <@ Day1.Part2.Parsing.parseInput input @>
        test <@ Day1.Part2.Domain.solve input = expected @>

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
        let input = checkOk <@ Day2.Part1.Parsing.parseInput input @>
        test <@ Day2.Part1.Domain.solve input = expected @>

    let ``part2 data`` =
        [| load sample 12
           loadInput "Day2" 12881 |]

    [<Theory; MemberData (nameof ``part2 data``)>]
    let part2 (input: string list) (expected: int) =
        let input = checkOk <@ Day2.Part2.Parsing.parseInput input @>
        test <@ Day2.Part2.Domain.solve input = expected @>

module Day3 =
    let sample = "\
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"

    let ``part1 data`` =
        [| load sample 157
           loadInput "day3" 8515 |]

    [<Theory; MemberData (nameof ``part1 data``)>]
    let part1 (input: string list) (expected: int) =
        let input = checkOk <@ Day3.Part1.Parsing.parseInput input @>
        test <@ Day3.Part1.Domain.solve input = Ok expected @>

    let ``part2 data`` =
        [| load sample 70
           loadInput "Day3" 2434 |]

    [<Theory; MemberData (nameof ``part2 data``)>]
    let part2 (input: string list) (expected: int) =
        let input = checkOk <@ Day3.Part2.Parsing.parseInput input @>
        test <@ Day3.Part2.Domain.solve input = Ok expected @>

module Day4 =
    let sample = "\
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"

    let ``part1 data`` =
        [| load sample 2
           loadInput "day4" 413 |]

    [<Theory; MemberData (nameof ``part1 data``)>]
    let part1 (input: string list) (expected: int) =
        let input = checkOk <@ Day4.Part1.Parsing.parseInput input @>
        test <@ Day4.Part1.Domain.solve input = expected @>

    let ``part2 data`` =
        [| load sample 4
           loadInput "Day4" 806 |]

    [<Theory; MemberData (nameof ``part2 data``)>]
    let part2 (input: string list) (expected: int) =
        let input = checkOk <@ Day4.Part2.Parsing.parseInput input @>
        test <@ Day4.Part2.Domain.solve input = expected @>
