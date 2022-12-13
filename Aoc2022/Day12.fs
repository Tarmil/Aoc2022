module Aoc2022.Day12

open System.Collections.Generic
open FSharpx.Collections
open FSharpx

module Common =

    module Domain =

        type IGraph<'node, 'weight> =
            abstract Neighbors: 'node -> seq<'node * 'weight>

        type DijkstraResult<'node, 'weight> =
            { path: NonEmptyList<'node>
              pathLength: 'weight }

        type Weight<'w when 'w : (static member Zero : 'w)
                        and 'w : (static member (+) : 'w * 'w -> 'w)> = 'w

        let inline dijkstra<'node, 'weight when Weight<'weight>>
                (graph: IGraph<'node, 'weight>) (source: 'node) (isDestination: 'node -> bool) =

            let prio = PriorityQueue()
            prio.Enqueue(NonEmptyList.singleton source, 'weight.Zero)
            let found = HashSet()

            let rec loop () =
                match prio.TryDequeue() with
                | true, currentPath, currentPathLength ->
                    let currentNode = currentPath.Head
                    if isDestination currentNode then
                        Ok { path = NonEmptyList.rev currentPath
                             pathLength = currentPathLength }
                    else
                        if found.Add(currentNode) then
                            for neighbor, neighborWeight in graph.Neighbors(currentNode) do
                                if not (found.Contains(neighbor)) then
                                    let neighborPath = NonEmptyList.cons neighbor currentPath
                                    let neighborPathLength = neighborWeight + currentPathLength
                                    prio.Enqueue(neighborPath, neighborPathLength)
                        loop ()
                | false, _, _ ->
                    Error "There's no path"

            loop ()

    module Parsing =

        let heightMap (input: string list) =
            input
            |> Seq.map (Seq.map (function
                | 'S' -> 0
                | 'E' -> 25
                | c -> int c - int 'a'))
            |> array2D

        let findChar (char: char) (input: string list) =
            match
                input
                |> Seq.map (Seq.tryFindIndex ((=) char))
                |> Seq.tryFindWithIndex Option.isSome
            with
            | Some (x, Some y) -> Ok (x, y)
            | _ -> Error $"Cannot find char {char}"

module Part1 =
    open Common.Domain

    module Domain =

        type Node = { x: int; y: int }

        type Input =
            { graph: IGraph<Node, int>
              start: Node
              finish: Node }

        let solve input =
            dijkstra input.graph input.start (fun n -> n = input.finish)
            |> Result.map (fun res -> res.pathLength)

    module Parsing =
        open Domain
        open Common.Parsing

        let loadGraph (input: string list) =
            let heightMap = heightMap input
            let height = heightMap.GetLength(1)
            let width = heightMap.GetLength(0)
            { new IGraph<Node, int> with
                member _.Neighbors({ x = x; y = y }) =
                    seq {
                        let currentLevel = heightMap[x, y]
                        if x > 0 && heightMap[x-1, y] - currentLevel <= 1 then
                            { x = x-1; y = y }, 1
                        if x < width-1 && heightMap[x+1, y] - currentLevel <= 1 then
                            { x = x + 1; y = y }, 1
                        if y > 0 && heightMap[x, y-1] - currentLevel <= 1 then
                            { x = x; y = y - 1 }, 1
                        if y < height-1 && heightMap[x, y+1] - currentLevel <= 1 then
                            { x = x; y = y + 1 }, 1
                    } }

        let loadInput (input: string list) =
            Result.result {
                let! startX, startY = findChar 'S' input
                let! finishX, finishY = findChar 'E' input
                return
                    { graph = loadGraph input
                      start = { x = startX; y = startY }
                      finish = { x = finishX; y = finishY } }
            }

    let solve input =
        Parsing.loadInput input
        |> Result.bind Domain.solve

module Part2 =
    open Common.Domain

    module Domain =

        type Node = { x: int; y: int; isStart: bool }

        type Input =
            { graph: IGraph<Node, int>
              finish: Node }

        let solve input =
            dijkstra input.graph input.finish (fun node -> node.isStart)
            |> Result.map (fun res -> res.pathLength)

    module Parsing =
        open Domain
        open Common.Parsing

        let loadGraph (input: string list) =
            let heightMap = heightMap input
            let height = heightMap.GetLength(1)
            let width = heightMap.GetLength(0)
            let toNode (x, y) =
                { x = x; y = y; isStart = heightMap[x, y] = 0 }
            { new IGraph<Node, int> with
                member _.Neighbors({ x = x; y = y }) =
                    seq {
                        let currentLevel = heightMap[x, y]
                        if x > 0 && heightMap[x-1, y] - currentLevel >= -1 then
                            toNode (x-1, y), 1
                        if x < width-1 && heightMap[x+1, y] - currentLevel >= -1 then
                            toNode (x+1, y), 1
                        if y > 0 && heightMap[x, y-1] - currentLevel >= -1 then
                            toNode (x, y-1), 1
                        if y < height-1 && heightMap[x, y+1] - currentLevel >= -1 then
                            toNode (x, y+1), 1
                    } }

        let loadInput input =
            Result.result {
                let! finishX, finishY = findChar 'E' input
                return { graph = loadGraph input
                         finish = { x = finishX; y = finishY; isStart = false } }
            }

    let solve input =
        Parsing.loadInput input
        |> Result.bind Domain.solve
