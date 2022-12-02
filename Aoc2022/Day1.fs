module Aoc2022.Day1

module Domain =

    let part1 (input: int list list) : int =
        input
        |> List.map List.sum
        |> List.max

    let part2 (input: int list list) : int =
        input
        |> List.map List.sum
        |> List.sortDescending
        |> List.take 3
        |> List.sum

module Parsing =

    let parseInput (input: string list) : int list list =
        let rec loop current input =
            match input with
            | [] -> [ List.rev current ]
            | "" :: rest -> List.rev current :: loop [] rest
            | item :: rest -> loop (int item :: current) rest

        loop [] input
