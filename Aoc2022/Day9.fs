module Aoc2022.Day9

open FSharpx.Collections

module Common =

    module Domain =

        type Pos = { x: int; y: int } with

            static member (+) (a: Pos, v: Vec) : Pos =
                { x = a.x + v.x; y = a.y + v.y }

            static member (-) (a: Pos, b: Pos) : Vec =
                { x = a.x - b.x; y = a.y - b.y }

        and Vec = { x: int; y: int }

        let normalize (v: Vec) : Vec =
            { x = sign v.x; y = sign v.y }

        let norm (v: Vec) : int =
            max (abs v.x) (abs v.y)

        let followKnot (leader: Pos) (follower: Pos) : Pos =
            let d = leader - follower
            if norm d > 1 then
                follower + normalize d
            else
                follower

        type Rope = { knots: Pos NonEmptyList }
        let makeRope knots = { knots = knots }

        let moveRopeByDir (dir: Vec) (rope: Rope) =
            let newHead = NonEmptyList.head rope.knots + dir

            NonEmptyList.tail rope.knots
            |> List.scan followKnot newHead
            |> NonEmptyList.ofList
            |> makeRope

        let origin : Pos = { x = 0; y = 0 }

        let emptyRope length =
            { knots = NonEmptyList.create origin [ for _ in 1..length-1 -> origin ] }

        type Instruction = { count: int; dir: Vec }

        let moveRopeByInstruction instruction (rope: Rope) (visited: Set<Pos>) =
            let initialState = {| rope = rope; visited = visited |}

            (initialState, {1..instruction.count})
            ||> Seq.fold (fun state _ ->
                let movedRope = moveRopeByDir instruction.dir state.rope
                let movedTail = NonEmptyList.last movedRope.knots
                {| rope = movedRope
                   visited = Set.add movedTail state.visited |})

        type Input = Instruction list

        let moveRopeByInput (input: Input) (rope: Rope) =
            let initialState = {| rope = rope
                                  visited = Set.singleton (NonEmptyList.last rope.knots) |}

            (initialState, input)
            ||> List.fold (fun state instruction ->
                moveRopeByInstruction instruction state.rope state.visited)

    module Parsing =
        open FParsec
        open Domain

        let pdir = anyOf "UDLR" |>> function
            | 'U' -> { x = 0; y = 1 }
            | 'D' -> { x = 0; y = -1 }
            | 'L' -> { x = -1; y = 0 }
            | 'R' -> { x = 1; y = 0 }
            | _ -> failwith "Impossible"

        let pinstruction =
            pipe3 pdir (skipChar ' ') pint32
                (fun dir _ count -> { count = count; dir = dir })

        let pinput = sepEndBy pinstruction skipNewline

        let parseInput text =
            match runParserOnString pinput () "input" text with
            | Success(x, _, _) -> Result.Ok x
            | Failure(e, _, _) -> Result.Error e

module Part1 =

    module Domain =
        open Common.Domain

        let solve (input: Input) =
            let rope = emptyRope 2
            let res = moveRopeByInput input rope
            Set.count res.visited

    let solve input =
        Common.Parsing.parseInput input
        |> Result.map Domain.solve

module Part2 =

    module Domain =
        open Common.Domain

        let solve (input: Input) =
            let rope = emptyRope 10
            let res = moveRopeByInput input rope
            Set.count res.visited

    let solve input =
        Common.Parsing.parseInput input
        |> Result.map Domain.solve
