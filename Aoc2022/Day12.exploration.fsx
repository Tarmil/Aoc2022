#load "Common.fs"
#r "nuget: FSharpx.Extras"

open System.Collections.Generic
open FSharpx.Collections

type IGraph<'node, 'weight> =
    abstract Neighbors: 'node -> seq<'node * 'weight>

let inline dijkstra (graph: IGraph<'node, 'weight>) (source: 'node) (dest: 'node) : 'node NonEmptyList * 'weight =
    let prio = PriorityQueue()
    prio.Enqueue(NonEmptyList.singleton source, LanguagePrimitives.GenericZero<'weight>)
    let found = HashSet()

    let mutable currentPath = Unchecked.defaultof<NonEmptyList<'node>>
    let mutable currentWeight = Unchecked.defaultof<'weight>
    let mutable res = None
    while res.IsNone && prio.TryDequeue(&currentPath, &currentWeight) do
        let currentNode = currentPath.Head
        if currentNode = dest then
            res <- Some (currentPath, currentWeight)
        elif found.Add(currentNode) then
            for neighbor, neighborWeight in graph.Neighbors(currentNode) do
                if not (found.Contains neighbor) then
                    prio.Enqueue(NonEmptyList.cons neighbor currentPath, neighborWeight + currentWeight)

    match res with
    | None -> failwith "There's no path!"
    | Some (path, weight) ->
        NonEmptyList.rev path, weight

let loadGraph (input: string list) =
    let graph =
        input
        |> Seq.map (Seq.map (function
            | 'S' -> 0
            | 'E' -> 26
            | c -> int c - int 'a'))
        |> array2D
    let height = graph.GetLength(1)
    let width = graph.GetLength(0)
    { new IGraph<int * int, int> with
        member _.Neighbors((x, y)) =
            seq {
                let currentLevel = graph[x, y]
                if x > 0 && graph[x-1, y] - currentLevel <= 1 then
                    (x-1, y), 1
                if x < width-1 && graph[x+1, y] - currentLevel <= 1 then
                    (x+1, y), 1
                if y > 0 && graph[x, y-1] - currentLevel <= 1 then
                    (x, y-1), 1
                if y < height-1 && graph[x, y+1] - currentLevel <= 1 then
                    (x, y+1), 1
            } }

let findChar (char: char) (input: string list) =
    input
    |> Seq.map (Seq.tryFindIndex ((=) char))
    |> Seq.tryFindWithIndex Option.isSome
    |> Option.bind (function
        | x, Some y -> Some (x, y)
        | _, None -> None)

let sample = load "\
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"
let (Some sampleStart) = findChar 'S' sample
let (Some sampleEnd) = findChar 'E' sample

dijkstra (sample |> loadGraph) sampleStart sampleEnd

let input = loadInput "day12"
let (Some inputStart) = findChar 'S' sample
let (Some inputEnd) = findChar 'E' input

dijkstra (loadGraph input) inputStart inputEnd

let printPath input nstart nend =
    let path, _ = dijkstra (loadGraph input) nstart nend
    let path = Set path
    input |> Seq.iteri (fun x line ->
        line |> Seq.iteri (fun y char ->
            System.Console.BackgroundColor <-
                if path.Contains(x, y) then
                    System.ConsoleColor.DarkGreen
                else
                    System.ConsoleColor.Black
            printf "%c" char)
        System.Console.BackgroundColor <- System.ConsoleColor.Black
        printfn "")

printPath sample sampleStart sampleEnd
printPath input inputStart inputEnd

let inline dijkstra2 (graph: IGraph<'node, 'weight>) (source: 'node) (isDest: 'node -> bool) : 'node NonEmptyList * 'weight =
    let prio = PriorityQueue()
    prio.Enqueue(NonEmptyList.singleton source, LanguagePrimitives.GenericZero<'weight>)
    let found = HashSet()

    let mutable currentPath = Unchecked.defaultof<NonEmptyList<'node>>
    let mutable currentWeight = Unchecked.defaultof<'weight>
    let mutable res = None
    while res.IsNone && prio.TryDequeue(&currentPath, &currentWeight) do
        let currentNode = currentPath.Head
        if isDest currentNode then
            res <- Some (currentPath, currentWeight)
        elif found.Add(currentNode) then
            for neighbor, neighborWeight in graph.Neighbors(currentNode) do
                if not (found.Contains neighbor) then
                    prio.Enqueue(NonEmptyList.cons neighbor currentPath, neighborWeight + currentWeight)

    match res with
    | None -> failwith "There's no path!"
    | Some (path, weight) ->
        NonEmptyList.rev path, weight


let loadGraph2 (input: string list) =
    let graph =
        input
        |> Seq.map (Seq.map (function
            | 'S' -> 0
            | 'E' -> 25
            | c -> int c - int 'a'))
        |> array2D
    let height = graph.GetLength(1)
    let width = graph.GetLength(0)
    let toNode (x, y) =
        (x, y, graph[x, y] = 0)
    { new IGraph<int * int * bool, int> with
        member _.Neighbors((x, y, _)) =
            seq {
                let currentLevel = graph[x, y]
                if x > 0 && graph[x-1, y] - currentLevel >= -1 then
                    toNode (x-1, y), 1
                if x < width-1 && graph[x+1, y] - currentLevel >= -1 then
                    toNode (x+1, y), 1
                if y > 0 && graph[x, y-1] - currentLevel >= -1 then
                    toNode (x, y-1), 1
                if y < height-1 && graph[x, y+1] - currentLevel >= -1 then
                    toNode (x, y+1), 1
            } }

let part2 input =
    let graph = loadGraph2 input
    let (Some(sx, sy)) = findChar 'E' input
    let isDest (_, _, d) = d
    dijkstra2 graph (sx, sy, false) isDest

part2 sample
part2 (loadInput "day12")