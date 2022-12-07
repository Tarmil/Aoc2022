module Aoc2022.Day6

open FSharpx

module Common =

    let solve markerSize (input: string) =
        input
        |> Seq.windowed markerSize
        |> Seq.tryFindIndex (fun xs ->
            Set.count (Set xs) = markerSize)
        |> Result.ofOption "No marker found"
        |> Result.map (fun markerStart -> markerStart + markerSize)

module Part1 =

    let solve input = Common.solve 4 input

module Part2 =

    let solve input = Common.solve 14 input
