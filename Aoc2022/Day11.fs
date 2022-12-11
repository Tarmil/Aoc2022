module Aoc2022.Day11

module Common =

    module Domain =

        type [<Struct>] MonkeyId = MonkeyId of int

        type [<Struct>] Item = Item of int64

        type Operand = Old | Const of int64

        type Monkey =
            { id: MonkeyId
              initItems: Item list
              operation: Item -> Item
              divisor: int64
              throwTo: Item -> MonkeyId }

        type State = Map<MonkeyId, Item list>

        let initState monkeys =
            Map [ for monkey in monkeys -> (monkey.id, monkey.initItems) ]

        let addOrUpdate key value update map =
            map |> Map.change key (function
                | None -> Some value
                | Some existing -> Some (update existing))

        let moveItemTo (target: MonkeyId) (state: State) (item: Item) =
            state |> addOrUpdate target [item] (fun items -> items @ [item])

        let runMonkeyRound (correctItem: Item -> Item) (monkeyItems: Item list) (state: State) (monkey: Monkey) =
            let state = Map.remove monkey.id state
            (state, monkeyItems)
            ||> List.fold (fun state item ->
                let inspectedItem = monkey.operation item |> correctItem
                let target = monkey.throwTo inspectedItem
                inspectedItem |> moveItemTo target state)

        type Manipulated = Map<MonkeyId, int64>

        let addManipulated (monkeyId: MonkeyId) (count: int64) (manipulated: Manipulated) =
            manipulated |> addOrUpdate monkeyId count ((+) count)

        let runFullRound (correctItem: Item -> Item) (state: State) (alreadyManipulated: Manipulated) (monkeys: Monkey list) =
            ((state, alreadyManipulated), monkeys)
            ||> Seq.fold (fun (state, manipulated) monkey ->
                match Map.tryFind monkey.id state with
                | None ->
                    (state, manipulated)
                | Some monkeyItems ->
                    let newState = runMonkeyRound correctItem monkeyItems state monkey
                    let manipulatedThisRound = int64 (List.length monkeyItems)
                    let newManipulated = addManipulated monkey.id manipulatedThisRound manipulated
                    newState, newManipulated)

        let runFull monkeys correctItem roundCount : State * Manipulated =
            let state = initState monkeys
            ((state, Map []), {1..roundCount})
            ||> Seq.fold (fun (state, manipulated) _ ->
                runFullRound correctItem state manipulated monkeys)

        let getTop2 (manipulated: Manipulated) =
            Map.values manipulated
            |> Seq.sortDescending
            |> Seq.truncate 2
            |> Seq.reduce (*)

    module Parsing =
        open FParsec
        open Domain

        let pmonkeyId = pint32 |>> MonkeyId

        let pmonkeyIdLine = skipString "Monkey " >>. pmonkeyId .>> skipString ":" .>> skipNewline

        let pstartingItems =
            let pitem = pint64 |>> Item
            let pitemList = sepBy pitem (skipString ", ")
            skipString "  Starting items: " >>. pitemList .>> skipNewline

        let poperation =
            let pold = skipString "old" >>% Old
            let pconst = pint64 |>> Const
            let poperand = pold <|> pconst

            let pplus = skipString "+ " >>% (+)
            let ptimes = skipString "* " >>% (*)
            let poperator = pplus <|> ptimes

            skipString "  Operation: new = old " >>. poperator .>>. poperand .>> skipNewline
            |>> fun (operator, operand) ->
                match operand with
                | Old -> fun (Item x) -> Item (operator x x)
                | Const c -> fun (Item x) -> Item (operator x c)

        let ptest = skipString "  Test: divisible by " >>. pint64 .>> skipNewline

        let pifTrue = skipString "    If true: throw to monkey " >>. pmonkeyId .>> skipNewline

        let pifFalse = skipString "    If false: throw to monkey " >>. pmonkeyId .>> skipNewline

        let pmonkey = parse {
            let! monkeyId = pmonkeyIdLine
            let! startingItems = pstartingItems
            let! operation = poperation
            let! test = ptest
            let! ifTrue = pifTrue
            let! ifFalse = pifFalse
            return
                { id = monkeyId
                  initItems = startingItems
                  operation = operation
                  divisor = test
                  throwTo = fun (Item x) ->
                      if x % test = 0 then ifTrue else ifFalse }
        }

        let pmonkeys = sepEndBy pmonkey skipNewline

        let parseInput input =
            match runParserOnString pmonkeys () "input" input with
            | Success(x, _, _) -> Result.Ok x
            | Failure(e, _, _) -> Result.Error e

module Part1 =

    module Domain =
        open Common.Domain

        let solve monkeys =
            let correctItem (Item item) =
                Item (item / 3L)
            let _finalState, manipulated = runFull monkeys correctItem 20
            getTop2 manipulated

    let solve input =
        Common.Parsing.parseInput input
        |> Result.map Domain.solve

module Part2 =

    module Domain =
        open Common.Domain

        let solve monkeys =
            let modulo =
                monkeys
                |> Seq.map (fun monkey -> monkey.divisor)
                |> Seq.reduce (*)
            let correctItem (Item item) =
                Item (item % modulo)
            let _finalState, manipulated = runFull monkeys correctItem 10_000
            getTop2 manipulated

    let solve input =
        Common.Parsing.parseInput input
        |> Result.map Domain.solve
