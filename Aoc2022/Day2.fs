module Aoc2022.Day2

open FSharpx

module Common =

    module Domain =

        type Shape = Rock | Paper | Scissors

        let shapeScore shape =
            match shape with
            | Rock -> 1
            | Paper -> 2
            | Scissors -> 3

        type Outcome = Win | Draw | Lose

        let outcomeScore outcome =
            match outcome with
            | Win -> 6
            | Draw -> 3
            | Lose -> 0

        let roundOutcome opponent player =
            match opponent, player with
            | Rock, Rock
            | Paper, Paper
            | Scissors, Scissors -> Draw
            | Rock, Paper
            | Paper, Scissors
            | Scissors, Rock -> Win
            | Rock, Scissors
            | Scissors, Paper
            | Paper, Rock -> Lose

        let shapeToPlayForOutcome opponent outcome =
            match opponent, outcome with
            | Rock, Win
            | Paper, Draw
            | Scissors, Lose -> Paper
            | Paper, Win
            | Scissors, Draw
            | Rock, Lose -> Scissors
            | Scissors, Win
            | Rock, Draw
            | Paper, Lose -> Rock

    module Parsing =
        open Domain

        let (|OpponentShape|_|) char =
            match char with
            | 'A' -> Some Rock
            | 'B' -> Some Paper
            | 'C' -> Some Scissors
            | _ -> None

module Part1 =
    open Common.Domain

    module Domain =

        type Round = Round of opponent: Shape * player: Shape

        let roundScore (Round (opponent, player)) =
            outcomeScore (roundOutcome opponent player)
            + shapeScore player

        let solve (input: Round list) : int =
            input |> List.sumBy roundScore

    module Parsing =
        open Common.Parsing
        open Domain

        let (|PlayerShape|_|) char =
            match char with
            | 'X' -> Some Rock
            | 'Y' -> Some Paper
            | 'Z' -> Some Scissors
            | _ -> None

        let tryParseRound (round: string) =
            match List.ofSeq round with
            | [ OpponentShape o; ' '; PlayerShape p ] -> Ok (o, p)
            | _ -> Error $"Invalid round description: {round}"

        let parseInput (input: string list) : Result<Round list, string> =
            Result.result {
                let! rounds =
                    input
                    |> List.map tryParseRound
                    |> Result.sequence
                return List.map Round rounds
            }

    let solve input =
        input
        |> Parsing.parseInput
        |> Result.map Domain.solve

module Part2 =
    open Common.Domain

    module Domain =

        type Round = Round of opponent: Shape * outcome: Outcome

        let roundScore (Round (opponent, outcome)) =
            shapeScore (shapeToPlayForOutcome opponent outcome)
            + outcomeScore outcome

        let solve (input: Round list) : int =
            input |> List.sumBy roundScore

    module Parsing =
        open Common.Parsing
        open Domain

        let (|Outcome|_|) char =
            match char with
            | 'X' -> Some Lose
            | 'Y' -> Some Draw
            | 'Z' -> Some Win
            | _ -> None

        let tryParseRound (round: string) =
            match List.ofSeq round with
            | [ OpponentShape opp; ' '; Outcome out ] -> Ok (opp, out)
            | _ -> Error $"Invalid round description: {round}"

        let parseInput input =
            Result.result {
                let! rounds =
                    input
                    |> List.map tryParseRound
                    |> Result.sequence
                return List.map Round rounds
            }

    let solve input =
        input
        |> Parsing.parseInput
        |> Result.map Domain.solve
