module Aoc2022.Day2

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

let (|OpponentShape|_|) char =
    match char with
    | 'A' -> Some Rock
    | 'B' -> Some Paper
    | 'C' -> Some Scissors
    | _ -> None

let (|PlayerShape|_|) char =
    match char with
    | 'X' -> Some Rock
    | 'Y' -> Some Paper
    | 'Z' -> Some Scissors
    | _ -> None

let (|Outcome|_|) char =
    match char with
    | 'X' -> Some Lose
    | 'Y' -> Some Draw
    | 'Z' -> Some Win
    | _ -> None

module Part1 =

    module Domain =

        let roundScore opponent player =
            outcomeScore (roundOutcome opponent player)
            + shapeScore player

        let solve (input: (Shape * Shape) list) : int =
            input
            |> List.sumBy (fun (opponent, player) ->
                roundScore opponent player)

    module Parsing =

        let tryParseRound (round: string) =
            match List.ofSeq round with
            | [ OpponentShape o; ' '; PlayerShape p ] -> Some (o, p)
            | _ -> None

        let parseInput (input: string list) =
            input |> List.choose tryParseRound

module Part2 =

    module Domain =

        let roundScore opponent outcome =
            shapeScore (shapeToPlayForOutcome opponent outcome)
            + outcomeScore outcome

        let solve (input: (Shape * Outcome) list) : int =
            input
            |> List.sumBy (fun (opponent, outcome) -> roundScore opponent outcome)

    module Parsing =

        let tryParseRound (round: string) =
            match List.ofSeq round with
            | [ OpponentShape opp; ' '; Outcome out ] -> Some (opp, out)
            | _ -> None

        let parseInput input =
            input |> List.choose tryParseRound
