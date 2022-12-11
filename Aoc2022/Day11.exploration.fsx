#load "Common.fs"
#r "nuget: FParsec"

open FParsec

type Operand = Old | Value of int64

type Monkey =
    { id: int
      initItems: int64 list
      operation: int64 -> int64
      divisor: int64
      throwTo: int64 -> int }

//let pbigint =
//    many1Chars (anyOf "0123456789")
//    |>> fun chars ->
//        let bytes = [| for c in chars -> byte c - byte '0' |]
//        bigint bytes

let pmonkeyId = skipString "Monkey " >>. pint32 .>> skipRestOfLine true
let pintList = sepBy pint64 (skipString ", ")
let pstarting = skipString "  Starting items: " >>. pintList .>> skipNewline
let poperand = (skipString "old" >>% Old) <|> (pint64 |>> Value)
let poperator = (skipString "+ " >>% (+)) <|> (skipString "* " >>% (*))
let poperation = skipString "  Operation: new = old " >>. poperator .>>. poperand .>> skipNewline
let ptest = skipString "  Test: divisible by " >>. pint64 .>> skipNewline
let pifTrue = skipString "    If true: throw to monkey " >>. pint32 .>> skipNewline
let pifFalse = skipString "    If false: throw to monkey " >>. pint32 .>> skipNewline

let pmonkey =
    parse {
        let! monkeyId = pmonkeyId
        let! startingItems = pstarting
        let! operation = poperation
        let! test = ptest
        let! ifTrue = pifTrue
        let! ifFalse = pifFalse
        return
            { id = monkeyId
              initItems = startingItems
              operation =
                  match operation with
                  | op, Old -> fun x -> op x x
                  | op, Value v -> fun x -> op x v
              divisor = test
              throwTo = fun x ->
                  if x % test = 0 then ifTrue else ifFalse }
    }

let pmonkeys = sepEndBy pmonkey skipNewline

let parseMonkeys input =
    match runParserOnString pmonkeys () "" input with
    | Success(x, _, _) -> x
    | Failure(e, _, _) -> failwith e

let sample = "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
"
let sampleMonkeys = parseMonkeys sample
let inputMonkeys = parseMonkeys (loadInputAsOneString "day11")

let initState monkeys =
    Map [ for monkey in monkeys -> (monkey.id, monkey.initItems) ]

let runMonkey items state monkey =
    let state = Map.remove monkey.id state
    (state, items)
    ||> List.fold (fun state item ->
        let item = monkey.operation item / 3L
        let target = monkey.throwTo item
        state |> Map.change target (function
            | None -> Some [item]
            | Some items -> Some (items @ [item])))

//runMonkey (initState sampleMonkeys) (List.head sampleMonkeys)

let runRound (state, manipulated) monkeys =
    ((state, manipulated), monkeys)
    ||> Seq.fold (fun (state, manipulated) monkey ->
        match Map.tryFind monkey.id state with
        | None -> (state, manipulated)
        | Some items ->
            let newState = runMonkey items state monkey
//            let newManipulated = Map.add monkey.id (List.length items) manipulated
            let manipulatedThisRound = List.length items
            let newManipulated =
                manipulated
                |> Map.change monkey.id (function
                    | None -> Some manipulatedThisRound
                    | Some manipulatedBefore -> Some (manipulatedBefore + manipulatedThisRound))
            newState, newManipulated)

let s = runRound (initState sampleMonkeys, Map []) sampleMonkeys
//let s = runRound s sampleMonkeys

let st = runRound (initState inputMonkeys, Map []) inputMonkeys
//let st = runRound st inputMonkeys

//let mergeMaps (combine: 'v -> 'v -> 'v) (m1: Map<'k, 'v>) (m2: Map<'k, 'v>) =
//    (m1, m2)
//    ||> Map.fold (fun m k v2 ->
//        m |> Map.change k (function
//            | None -> Some v2
//            | Some v1 -> Some (combine v1 v2)))
//
//mergeMaps (+) (Map [1, "1"; 2, "2"]) (Map [2, "B"; 3, "C"])

let run monkeys =
    let state = initState monkeys
    ((state, Map []), {1..20})
    ||> Seq.fold (fun state _ ->
        runRound state monkeys)

run sampleMonkeys
run inputMonkeys

let part1 monkeys =
    let _finalState, manipulated = run monkeys
    Map.values manipulated
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.reduce (*)

part1 sampleMonkeys
part1 inputMonkeys




let runMonkey2 modulo items state monkey =
    let state = Map.remove monkey.id state
    (state, items)
    ||> List.fold (fun state item ->
        let item = monkey.operation item % modulo
        let target = monkey.throwTo item
        state |> Map.change target (function
            | None -> Some [item]
            | Some items -> Some (items @ [item])))

let runRound2 modulo (state, manipulated) monkeys =
    ((state, manipulated), monkeys)
    ||> Seq.fold (fun (state, manipulated) monkey ->
        match Map.tryFind monkey.id state with
        | None -> (state, manipulated)
        | Some items ->
            let newState = runMonkey2 modulo items state monkey
//            let newManipulated = Map.add monkey.id (List.length items) manipulated
            let manipulatedThisRound = int64 (List.length items)
            let newManipulated =
                manipulated
                |> Map.change monkey.id (function
                    | None -> Some manipulatedThisRound
                    | Some manipulatedBefore -> Some (manipulatedBefore + manipulatedThisRound))
            newState, newManipulated)

let run2 monkeys =
    let state = initState monkeys
    let modulo =
        monkeys
        |> Seq.map (fun m -> m.divisor)
        |> Seq.reduce (*)
    ((state, Map []), {1..10000})
    ||> Seq.fold (fun state i ->
        if i % 500 = 0 then
            printfn "%A" state
        runRound2 modulo state monkeys)

let part2 monkeys =
    let _finalState, manipulated = run2 monkeys
    Map.values manipulated
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.reduce (*)

part2 sampleMonkeys
part2 inputMonkeys