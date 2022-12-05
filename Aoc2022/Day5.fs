module Aoc2022.Day5

open FSharpx
open FSharpx.Collections

module Common =

    module Domain =

        type Crate = Crate of char

        type Stack = Crate list

        type MoveAction =
            { count: int
              from: int
              to': int }

        type Input =
            { stacks: Stack list
              actions: MoveAction list }

    module Parsing =
        open FParsec
        open Domain

        let pcrate =
            anyChar
            |> between (skipChar '[') (skipChar ']')
            |>> Crate

        let pnoCrate = skipString "   "

        let pmaybeCrate = (pcrate |>> Some) <|> (pnoCrate >>% None)

        let pcrateLine = sepBy1 pmaybeCrate (skipChar ' ')

        let pcrateLines = sepEndBy pcrateLine skipNewline

        let crateLinesToStacks crateLines =
            crateLines
            |> List.transpose
            |> List.map List.catOptions

        let pstacks = pcrateLines |>> crateLinesToStacks

        let pseparator =
            skipRestOfLine true // Stack numbers
            >>. skipNewline     // Empty line

        let pactionLine =
            pipe3 (skipString "move " >>. pint32)
                  (skipString " from " >>. pint32)
                  (skipString " to " >>. pint32)
                  (fun count from to' ->
                    { count = count; from = from - 1; to' = to' - 1 })

        let paction = sepEndBy pactionLine skipNewline

        let pactions = sepEndBy pactionLine skipNewline

        let pinput =
            pipe3 pstacks pseparator pactions
                (fun stacks () actions -> { stacks = stacks; actions = actions })

        let parseInput (input: string) =
            match runParserOnString pinput () "input" input with
            | Success(x, _, _) -> Result.Ok x
            | Failure(e, _, _) -> Result.Error e

module Part1 =
    open Common.Domain

    module Domain =

        let move count (fromStack: Stack) (toStack: Stack) =
            let toMove, restFromStack = List.splitAt count fromStack
            restFromStack, List.rev toMove @ toStack

        let solve (input: Input) =
            let stacks = Array.ofList input.stacks
            for action in input.actions do
                let newFrom, newTo = move action.count stacks[action.from] stacks[action.to']
                stacks[action.from] <- newFrom
                stacks[action.to'] <- newTo
            stacks
            |> Seq.mapi (fun i stack ->
                match stack with
                | Crate c :: _ -> Ok (string c)
                | [] -> Error $"Stack #{i} is empty")
            |> List.ofSeq
            |> Result.sequence
            |> Result.map (String.concat "")

    module Parsing =

        let parseInput input = Common.Parsing.parseInput input

    let solve input =
        Parsing.parseInput input
        |> Result.bind Domain.solve

module Part2 =
    open Common.Domain

    module Domain =

        let move count (fromStack: Stack) (toStack: Stack) =
            let toMove, restFromStack = List.splitAt count fromStack
            restFromStack, toMove @ toStack

        let solve (input: Input) =
            let stacks = Array.ofList input.stacks
            for action in input.actions do
                let newFrom, newTo = move action.count stacks[action.from] stacks[action.to']
                stacks[action.from] <- newFrom
                stacks[action.to'] <- newTo
            stacks
            |> Seq.mapi (fun i stack ->
                match stack with
                | Crate c :: _ -> Ok (string c)
                | [] -> Error $"Stack #{i} is empty")
            |> List.ofSeq
            |> Result.sequence
            |> Result.map (String.concat "")

    module Parsing =

        let parseInput input = Common.Parsing.parseInput input

    let solve input =
        Parsing.parseInput input
        |> Result.bind Domain.solve
