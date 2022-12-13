module Aoc2022.Tests

open Xunit
open Swensen.Unquote

module InputAsList =

    let load (input: string) expected =
        [| box (load input); box expected |]

    let loadInput (name: string) expected =
        [| box (loadInput name); box expected |]

module InputAsString =

    let load (input: string) expected =
        [| box input; box expected |]

    let loadInput (name: string) expected =
        [| box (loadInputAsOneString name); box expected |]

module Day1 =
    open InputAsList

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
        test <@ Day1.Part1.solve input = Ok expected @>

    let ``part2 data`` =
        [| load sample 45000
           loadInput "day1" 207410 |]

    [<Theory; MemberData (nameof ``part2 data``)>]
    let part2 (input: string list) (expected: int) =
        test <@ Day1.Part2.solve input = Ok expected @>

module Day2 =
    open InputAsList

    let sample = "\
A Y
B X
C Z"

    let ``part1 data`` =
        [| load sample 15
           loadInput "day2" 13682 |]

    [<Theory; MemberData (nameof ``part1 data``)>]
    let part1 (input: string list) (expected: int) =
        test <@ Day2.Part1.solve input = Ok expected @>

    let ``part2 data`` =
        [| load sample 12
           loadInput "day2" 12881 |]

    [<Theory; MemberData (nameof ``part2 data``)>]
    let part2 (input: string list) (expected: int) =
        test <@ Day2.Part2.solve input = Ok expected @>

module Day3 =
    open InputAsList

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
        test <@ Day3.Part1.solve input = Ok expected @>

    let ``part2 data`` =
        [| load sample 70
           loadInput "day3" 2434 |]

    [<Theory; MemberData (nameof ``part2 data``)>]
    let part2 (input: string list) (expected: int) =
        test <@ Day3.Part2.solve input = Ok expected @>

module Day4 =
    open InputAsList

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
        test <@ Day4.Part1.solve input = Ok expected @>

    let ``part2 data`` =
        [| load sample 4
           loadInput "day4" 806 |]

    [<Theory; MemberData (nameof ``part2 data``)>]
    let part2 (input: string list) (expected: int) =
        test <@ Day4.Part2.solve input = Ok expected @>

module Day5 =
    open InputAsString

    let sample = "    [D]    \n\
[N] [C]    \n\
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"

    let ``part1 data`` =
        [| load sample "CMZ"
           loadInput "day5" "FJSRQCFTN" |]

    [<Theory; MemberData (nameof ``part1 data``)>]
    let part1 (input: string) (expected: string) =
        test <@ Day5.Part1.solve input = Ok expected @>

    let ``part2 data`` =
        [| load sample "MCD"
           loadInput "day5" "CJVLJQPHS" |]

    [<Theory; MemberData (nameof ``part2 data``)>]
    let part2 (input: string) (expected: string) =
        test <@ Day5.Part2.solve input = Ok expected @>

module Day6 =
    open InputAsString

    let samples =
        [| "mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7, 19
           "bvwbjplbgvbhsrlpgdmjqwftvncz", 5, 23
           "nppdvjthqldpwncqszvftbrmjlhg", 6, 23
           "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10, 29
           "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11, 26 |]

    let ``part1 data`` =
        [| for sample, expected, _ in samples do
                load sample expected
           loadInput "day6" 1658 |]

    [<Theory; MemberData (nameof ``part1 data``)>]
    let part1 (input: string) (expected: int) =
        test <@ Day6.Part1.solve input = Ok expected @>

    let ``part2 data`` =
        [| for sample, _, expected in samples do
                load sample expected
           loadInput "day6" 2260 |]

    [<Theory; MemberData (nameof ``part2 data``)>]
    let part2 (input: string) (expected: int) =
        test <@ Day6.Part2.solve input = Ok expected @>

module Day7 =
    open InputAsString

    let sample = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"

    let ``part1 data`` =
        [| load sample 95437
           loadInput "day7" 1770595 |]

    [<Theory; MemberData (nameof ``part1 data``)>]
    let part1 (input: string) (expected: int) =
        test <@ Day7.Part1.solve input = Ok expected @>

    let ``part2 data`` =
        [| load sample 24933642
           loadInput "day7" 2195372 |]

    [<Theory; MemberData (nameof ``part2 data``)>]
    let part2 (input: string) (expected: int) =
        test <@ Day7.Part2.solve input = Ok expected @>

module Day9 =
    open InputAsString

    let sample = "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"

    let ``part1 data`` =
        [| load sample 13
           loadInput "day9" 6057 |]

    [<Theory; MemberData (nameof ``part1 data``)>]
    let part1 (input: string) (expected: int) =
        test <@ Day9.Part1.solve input = Ok expected @>

    let sample2 = "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"

    let ``part2 data`` =
        [| load sample 1
           load sample2 36
           loadInput "day9" 2514 |]

    [<Theory; MemberData (nameof ``part2 data``)>]
    let part2 (input: string) (expected: int) =
        test <@ Day9.Part2.solve input = Ok expected @>

module Day10 =
    open InputAsString

    let ``part1 data`` =
        [| loadInput "day10.sample" 13140
           loadInput "day10" 14560 |]

    [<Theory; MemberData (nameof ``part1 data``)>]
    let part1 (input: string) (expected: int) =
        test <@ Day10.Part1.solve input = Ok expected @>

    let ``part2 data`` =
        [| loadInput "day10.sample" "\
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######....."
           loadInput "day10" "\
####.#..#.###..#..#.####.###..#..#.####.
#....#.#..#..#.#..#.#....#..#.#..#....#.
###..##...#..#.####.###..#..#.#..#...#..
#....#.#..###..#..#.#....###..#..#..#...
#....#.#..#.#..#..#.#....#....#..#.#....
####.#..#.#..#.#..#.####.#.....##..####." |]

    [<Theory; MemberData (nameof ``part2 data``)>]
    let part2 (input: string) (expected: string) =
        test <@ Day10.Part2.solve input = Ok expected @>

module Day11 =
    open InputAsString

    let ``part1 data`` =
        [| loadInput "day11.sample" 10605L
           loadInput "day11" 55930L |]

    [<Theory; MemberData (nameof ``part1 data``)>]
    let part1 (input: string) (expected: int64) =
        test <@ Day11.Part1.solve input = Ok expected @>

    let ``part2 data`` =
        [| loadInput "day11.sample" 2713310158L
           loadInput "day11" 14636993466L |]

    [<Theory; MemberData (nameof ``part2 data``)>]
    let part2 (input: string) (expected: int64) =
        test <@ Day11.Part2.solve input = Ok expected @>

module Day13 =
    open InputAsString

    let sample = "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
"

    let ``part1 data`` =
        [| load sample 13
           loadInput "day13" 5843 |]

    [<Theory; MemberData (nameof ``part1 data``)>]
    let part1 (input: string) (expected: int) =
        test <@ Day13.Part1.solve input = Ok expected @>

    let ``part2 data`` =
        [| load sample 140
           loadInput "day13" 26289 |]

    [<Theory; MemberData (nameof ``part2 data``)>]
    let part2 (input: string) (expected: int) =
        test <@ Day13.Part2.solve input = Ok expected @>
