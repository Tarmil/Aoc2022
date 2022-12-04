module Aoc2022.Day1

open System
open FSharpx
open FSharpx.Collections

module Common =

    module Domain =

        type Elf = Elf of itemCalories: int list

    module Parsing =
        open Domain

        let parseItemCalories (s: string) =
            match Int32.TryParse(s) with
            | true, x -> Ok x
            | false, _ -> Error $"Invalid calories: {s}"

        let parseElf (s: string list) =
            Result.result {
                let! itemCalories =
                    s
                    |> List.map parseItemCalories
                    |> Result.sequence
                return Elf itemCalories
            }

        let parseInput (input: string list) : Result<Elf list, string> =
            let rec loop acc input =
                Result.result {
                    match List.split String.IsNullOrEmpty input with
                    | lastElf, [] ->
                        let! lastCalories = parseElf lastElf
                        return List.rev (lastCalories :: acc)
                    | elf, _ :: rest ->
                        let! elfCalories = parseElf elf
                        return! loop (elfCalories :: acc) rest
                }
            loop [] input

module Part1 =
    open Common.Domain

    module Domain =

        let solve (input: Elf list) : int =
            input
            |> List.map (fun (Elf itemCalories) -> List.sum itemCalories)
            |> List.max

    module Parsing =

        let parseInput input = Common.Parsing.parseInput input

    let solve input =
        input
        |> Parsing.parseInput
        |> Result.map Domain.solve

module Part2 =
    open Common.Domain

    module Domain =

        let solve (input: Elf list) : int =
            input
            |> List.map (fun (Elf itemCalories) -> List.sum itemCalories)
            |> List.sortDescending
            |> List.take 3
            |> List.sum

    module Parsing =

        let parseInput input = Common.Parsing.parseInput input

    let solve input =
        input
        |> Parsing.parseInput
        |> Result.map Domain.solve
