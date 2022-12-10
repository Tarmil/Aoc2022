module Aoc2022.Day10

open System

module Common =

    module Domain =

        type Instruction =
            | Addx of int
            | Noop

        type Program = Instruction list

        type State =
            { x: int
              xHistory: int list }

        let runInstruction state instr =
            match instr with
            | Noop ->
                { state with xHistory = state.x :: state.xHistory }
            | Addx v ->
                { x = state.x + v
                  xHistory = state.x :: state.x :: state.xHistory }

        let runProgram program =
            let finalState =
                List.fold runInstruction { x = 1; xHistory = [] } program
            finalState.xHistory
            |> Array.ofList
            |> Array.rev

    module Parsing =
        open FParsec
        open Domain

        let paddx = skipString "addx " >>. pint32 |>> Addx

        let pnoop = skipString "noop" >>% Noop

        let pinstruction = paddx <|> pnoop

        let pprogram = sepEndBy pinstruction skipNewline

        let parseInput input =
            match runParserOnString pprogram () "input" input with
            | Success(x, _, _) -> Result.Ok x
            | Failure(e, _, _) -> Result.Error e

module Part1 =
    open Common.Domain

    module Domain =

        let solve program =
            let xHistory = runProgram program
            if xHistory.Length < 220 then
                Error "Program too short"
            else
                xHistory[19] * 20
                + xHistory[59] * 60
                + xHistory[99] * 100
                + xHistory[139] * 140
                + xHistory[179] * 180
                + xHistory[219] * 220
                |> Ok

    let solve input =
        Common.Parsing.parseInput input
        |> Result.bind Domain.solve

module Part2 =
    open Common.Domain

    module Domain =

        let displayChar (screenPos: int) (x: int) =
            if abs (x - screenPos) <= 1 then
                '#'
            else
                '.'

        let displayLine (xHistory: int[]) =
            xHistory
            |> Array.mapi displayChar
            |> String

        let solve program =
            runProgram program
            |> Seq.chunkBySize 40
            |> Seq.map displayLine
            |> String.concat "\n"

    let solve input =
        Common.Parsing.parseInput input
        |> Result.map Domain.solve
