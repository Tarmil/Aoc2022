#if INTERACTIVE
#load "Common.fs"
#else
module Aoc2022.Day2
#endif

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

let roundScoreForPart1 opponent player =
    outcomeScore (roundOutcome opponent player)
    + shapeScore player

let part1 (input: (Shape * Shape) list) : int =
    input
    |> List.sumBy (fun (opponent, player) ->
        roundScoreForPart1 opponent player)

roundScoreForPart1 Rock Paper
roundScoreForPart1 Paper Rock
part1 [(Rock, Paper); (Paper, Rock)]

let (|Opponent|_|) char =
    match char with
    | 'A' -> Some Rock
    | 'B' -> Some Paper
    | 'C' -> Some Scissors
    | _ -> None

let (|Player|_|) char =
    match char with
    | 'X' -> Some Rock
    | 'Y' -> Some Paper
    | 'Z' -> Some Scissors
    | _ -> None

let tryParseRoundForPart1 (round: string) =
    match List.ofSeq round with
    | [ Opponent o; ' '; Player p ] -> Some (o, p)
    | _ -> None

tryParseRoundForPart1 "A Y"
tryParseRoundForPart1 "A A"

let parseInputForPart1 (input: string list) =
    input |> List.choose tryParseRoundForPart1

parseInputForPart1 ["A Y"; "A A"; "B Z"]

let example = load "A Y
B X
C Z"

example |> parseInputForPart1 |> part1

loadInput "day2" |> parseInputForPart1 |> part1

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

let roundScoreForPart2 opponent outcome =
    shapeScore (shapeToPlayForOutcome opponent outcome)
    + outcomeScore outcome

roundScoreForPart2 Scissors Draw
roundScoreForPart2 Rock Win

let part2 (input: (Shape * Outcome) list) : int =
    input
    |> List.sumBy (fun (opponent, outcome) -> roundScoreForPart2 opponent outcome)

part2 [(Scissors, Draw); (Rock, Win)]

let (|Outcome|_|) char =
    match char with
    | 'X' -> Some Lose
    | 'Y' -> Some Draw
    | 'Z' -> Some Win
    | _ -> None

let tryParseRoundForPart2 (round: string) =
    match List.ofSeq round with
    | [ Opponent opp; ' '; Outcome out ] -> Some (opp, out)
    | _ -> None

let parseInputForPart2 input =
    input |> List.choose tryParseRoundForPart2

example |> parseInputForPart2 |> part2

loadInput "day2" |> parseInputForPart2 |> part2