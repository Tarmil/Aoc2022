module Aoc2022.Day13

module Common =

    module Domain =

        [<StructuralEquality; CustomComparison>]
        type Item =
            | Number of int
            | List of Item list

            static member private compareItems i1 i2 =
                match i1, i2 with
                | Number x1, Number x2 -> compare x1 x2
                | List l1, List l2 -> Item.compareLists l1 l2
                | List l1, (Number _ as x2) -> Item.compareLists l1 [x2]
                | (Number _ as x1), List l2 -> Item.compareLists [x1] l2

            static member private compareLists l1 l2 =
                match l1, l2 with
                | [], [] -> 0
                | [], _ -> -1
                | _, [] -> 1
                | x1::rest1, x2::rest2 ->
                    match Item.compareItems x1 x2 with
                    | 0 -> Item.compareLists rest1 rest2
                    | res -> res

            interface System.IComparable<Item> with
                member this.CompareTo(other) = Item.compareItems this other

            interface System.IComparable with
                member this.CompareTo(obj) =
                    match obj with
                    | :? Item as other -> Item.compareItems this other
                    | _ -> 1

        type Input = (Item * Item) list

    module Parsing =
        open FParsec
        open Domain

        let pitem, rpitem = createParserForwardedToRef()

        let pitemNumber = pint32 |>> Number

        let pitemList =
            sepBy pitem (skipChar ',')
            |> between (skipChar '[') (skipChar ']')
            |>> List

        rpitem.Value <- (pitemNumber <|> pitemList)

        let pline = pitem .>> skipNewline

        let pitemPair = pline .>>. pline

        let pinput = sepEndBy pitemPair skipNewline

        let parseInput input =
            match runParserOnString pinput () "" input with
            | Success(x, _, _) -> Result.Ok x
            | Failure(e, _, _) -> Result.Error e

module Part1 =

    module Domain =
        open Common.Domain

        let solve (itemPairs: Input) =
            itemPairs
            |> List.mapi (fun i (x1, x2) ->
                match compare x1 x2 with
                | -1 -> i + 1
                | _ -> 0)
            |> List.sum

    let solve input =
        Common.Parsing.parseInput input
        |> Result.map Domain.solve

module Part2 =

    module Domain =
        open Common.Domain

        let sep2 = List [List [Number 2]]
        let sep6 = List [List [Number 6]]

        let sortInput (itemPairs: Input) =
            itemPairs
            |> List.collect (fun (x, y) -> [x; y])
            |> List.append [sep2; sep6]
            |> List.sort

        let solve itemPairs =
            let sorted = sortInput itemPairs
            let i1 = List.findIndex (fun x -> x = sep2) sorted
            let i2 = List.findIndex (fun x -> x = sep6) sorted
            (i1 + 1) * (i2 + 1)


    let solve input =
        Common.Parsing.parseInput input
        |> Result.map Domain.solve
