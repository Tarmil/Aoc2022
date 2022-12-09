#load "Common.fs"
#r "nuget: FParsec"

open FParsec

type Dir = Up | Down | Left | Right

type Positions = { head: int * int; tail: int * int }

let moveHead (x, y) dir =
    match dir with
    | Up -> (x, y + 1)
    | Down -> (x, y - 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

let follow { head = (hx, hy); tail = (tx, ty) } =
    let dx = hx - tx
    let dy = hy - ty
    if abs dx > 1 || abs dy > 1 then
        (tx + sign dx, ty + sign dy)
    else
        (tx, ty)

let moveBoth positions dir =
    let h = moveHead positions.head dir
    let t = follow { positions with head = h }
    { head = h; tail = t }

type Input = (Dir * int) list

let part1 input =
    let rec loop input positions visited =
        match input with
        | [] -> Set.count visited
        | (dir, count) :: rest ->
            ((positions, visited), {1..count})
            ||> Seq.fold (fun (positions, visited) _ ->
                let positions = moveBoth positions dir
                positions, Set.add positions.tail visited)
            ||> loop rest
    loop input { head = (0, 0); tail = (0, 0) } (Set.singleton (0, 0))

let pdir = anyOf "UDLR" |>> function
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | 'R' -> Right
    | _ -> failwith "Impossible"
let pline = pdir .>> skipChar ' ' .>>. pint32
let pinput = sepEndBy pline skipNewline
let parseInput input : Input =
    match runParserOnString pinput () "" input with
    | Success(x, _, _) -> x
    | Failure(e, _, _) -> failwith e

parseInput "\
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"
|> part1
loadInputAsOneString "day9" |> parseInput |> part1

type Rope = (int * int) list

let moveRope dir rope =
    let head::rest = rope
    let newHead = moveHead head dir
    let newTail, newRope =
        ((newHead, []), rest)
        ||> List.fold (fun (followingNode, rope) thisNode ->
            let newThisNode = follow { head = followingNode; tail = thisNode }
            (newThisNode, newThisNode :: rope))
    newTail, newHead :: List.rev newRope

[for i in 1..9 -> (0, 0)]
|> moveRope Right
|> snd |> moveRope Right
|> snd |> moveRope Right
|> snd |> moveRope Right
|> snd |> moveRope Right
|> snd |> moveRope Right
|> snd |> moveRope Right
|> snd |> moveRope Right
|> snd |> moveRope Right

let part2 input =
    let rec loop input rope visited =
        match input with
        | [] -> Set.count visited
        | (dir, count) :: rest ->
            ((rope, visited), {1..count})
            ||> Seq.fold (fun (rope, visited) _ ->
                let (newTail, newRope) = moveRope dir rope
                newRope, Set.add newTail visited)
            ||> loop rest
    loop input [for i in 0..9 -> (0, 0)] (Set.singleton (0, 0))

parseInput "\
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"
|> part2

parseInput "\
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
"
|> part2

loadInputAsOneString "day9" |> parseInput |> part2
