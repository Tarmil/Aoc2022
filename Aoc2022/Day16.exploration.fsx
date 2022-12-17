#load "Common.fs"
#r "nuget: FParsec"

open System.Collections.Generic
open FParsec

type Valve =
    { name: string
      flow: int
      neighbors: string list }

type Valves = IReadOnlyDictionary<string, Valve>

let pvalveName = anyString 2
let pvalveNames = sepBy pvalveName (skipString ", ")
let pneighbors =
        (skipString "; tunnels lead to valves " >>. pvalveNames)
        <|>
        (skipString "; tunnel leads to valve " >>. pvalveName |>> List.singleton)
let pValve =
    pipe3 (skipString "Valve " >>. pvalveName)
          (skipString " has flow rate=" >>. pint32)
          pneighbors
          (fun name flow neighbors -> { name = name; flow = flow; neighbors = neighbors })
let pinput =
    sepEndBy pValve skipNewline
    |>> fun l -> readOnlyDict [ for valve in l -> valve.name, valve ]

let parseInput input =
    match runParserOnString pinput () "" input with
    | Success(x, _, _) -> x
    | Failure(e, _, _) -> failwith e

let sample = parseInput "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
"

let input = parseInput (loadInputAsOneString "day16")


let rec dfs (valves: Valves) (alreadyOpen: Set<string>) (minutesLeft: int) (totalPressure: int) (pressurePerMinute: int) (currentValve: Valve) (comingFrom: Set<string>) =
    if minutesLeft = 0 then
//        printfn "%i" totalPressure
        totalPressure
    else
    printfn "%i %i %s %A %A" minutesLeft totalPressure currentValve.name alreadyOpen comingFrom
    let openThis =
        if Set.contains currentValve.name alreadyOpen then
            0
        else
            dfs valves (Set.add currentValve.name alreadyOpen) (minutesLeft - 1) (totalPressure + pressurePerMinute) (pressurePerMinute + currentValve.flow) currentValve (Set.singleton currentValve.name)
    let moveToNeighbor =
        let comingFrom = Set.add currentValve.name comingFrom
        currentValve.neighbors
        |> Seq.map (fun neighborName ->
            if Set.contains neighborName comingFrom then
                0
            else
                dfs valves alreadyOpen (minutesLeft - 1) (totalPressure + pressurePerMinute) pressurePerMinute valves[neighborName] comingFrom)
        |> Seq.max
    max openThis moveToNeighbor

// WAAAAYYYY too long!!!
//let part1 valves = dfs valves Set.empty 30 0 0 valves["AA"] (Set.singleton "AA")
//part1 sample

type Graph =
    { matrix: int[,]
      valves: IReadOnlyDictionary<string, int * Valve> }

let buildGraph (valves: Valves) (maxPath: int) =
    let valves =
        valves
        |> Seq.mapi (fun i (KeyValue(k, v)) -> k, (i, v))
        |> readOnlyDict

    let matrix = Array2D.create valves.Count valves.Count maxPath

    let rec loop (from: int) (queue: Queue<string * int>) =
        match queue.TryDequeue() with
        | true, (valveName, pathLength) ->
            let valveIndex, valve = valves[valveName]
            matrix[from, valveIndex] <- min pathLength matrix[from, valveIndex]
            for neighborName in valve.neighbors do
                let neighborIndex, _ = valves[neighborName]
                if neighborIndex <> from && matrix[from, neighborIndex] = maxPath then
                    queue.Enqueue(neighborName, pathLength + 1)
            loop from queue
        | false, _ -> ()

    for i, valve in valves.Values do
        let queue = Queue()
        for neighbor in valve.neighbors do
            queue.Enqueue(neighbor, 1)
        loop i queue

    for x in 0..valves.Count - 1 do
        for KeyValue(_, (y, valve)) in valves do
            if valve.flow > 0 then
                matrix[x, y] <- matrix[x, y] + 1
            else
                matrix[x, y] <- maxPath

    { matrix = matrix; valves = valves }

// printfn "%A" (buildGraph sample 30)
printfn "%A" (buildGraph input 30)

let rec dfs2 (graph: Graph) (visited: Set<string>) (path: string list) (currentValveIndex: int) (currentValve: Valve) (totalPressure: int) (pressurePerMinute: int) (minutesLeft: int) =
    match
        graph.valves.Values
        |> Seq.filter (fun (_, neighbor) -> not (Set.contains neighbor.name visited))
        |> Seq.choose (fun (neighborIndex, neighbor) ->
            let pathLength = graph.matrix[currentValveIndex, neighborIndex]
            if pathLength > minutesLeft then None else
            let newTotalPressure = totalPressure + pressurePerMinute * pathLength
            let newPressurePerMinute = pressurePerMinute + neighbor.flow
            let newMinutesLeft = minutesLeft - pathLength
            Some (dfs2 graph (Set.add currentValve.name visited) (neighbor.name :: path) neighborIndex neighbor newTotalPressure newPressurePerMinute newMinutesLeft))
        |> List.ofSeq
    with
    | [] -> path, totalPressure + pressurePerMinute * minutesLeft
    | l ->
        // printfn "PATHS: %A" l
        List.maxBy snd l

let part1 valves =
    let graph = buildGraph valves 31
    let currentValveIndex, currentValve = graph.valves["AA"]
    dfs2 graph (Set.singleton "AA") ["AA"] currentValveIndex currentValve 0 0 30

printfn "%A" <| part1 sample
printfn "%A" <| part1 input
