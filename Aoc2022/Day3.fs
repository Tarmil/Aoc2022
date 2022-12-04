module Aoc2022.Day3

open FSharpx

module Common =

    module Domain =

        type ItemType = private ItemType of char

        type Compartment = Compartment of Set<ItemType>

        type Rucksack = Rucksack of Compartment * Compartment

        let isItemCharLowPriority c =
            c >= 'a' && c <= 'z'

        let isItemCharHighPriority c =
            c >= 'A' && c <= 'Z'

        let makeItemType (c: char) =
            if isItemCharLowPriority c || isItemCharHighPriority c then
                Ok (ItemType c)
            else
                Error $"Invalid item type char: {c}"

        let itemTypePriority (ItemType c) =
            if isItemCharLowPriority c then
                int c - int 'a' + 1
            else
                int c - int 'A' + 27

    module Parsing =
        open Domain

        let tryParseCompartment (s: seq<char>) : Result<Compartment, string> =
            Result.result {
                let! items =
                    s
                    |> Seq.map makeItemType
                    |> List.ofSeq
                    |> Result.sequence
                return Compartment (Set items)
            }

        let tryParseRucksack (s: string) : Result<Rucksack, string> =
            Result.result {
                let compartmentSize = s.Length / 2
                let! firstCompartment = tryParseCompartment (Seq.take compartmentSize s)
                let! secondCompartment = tryParseCompartment (Seq.skip compartmentSize s)
                return Rucksack (firstCompartment, secondCompartment)
            }

module Part1 =
    open Common.Domain

    module Domain =

        let commonItemType (Rucksack (Compartment compartment1, Compartment compartment2)) =
            Set.intersect compartment1 compartment2
            |> Seq.tryExactlyOne
            |> Result.ofOption "Rucksack doesn't have exactly one common item type between its compartments"

        let solve (input: Rucksack list) : Result<int, string> =
            Result.result {
                let! itemTypes =
                    input
                    |> List.map commonItemType
                    |> Result.sequence
                return Seq.sumBy itemTypePriority itemTypes
            }

    module Parsing =
        open Common.Parsing

        let parseInput (input: string list) : Result<Rucksack list, string> =
            input
            |> List.map tryParseRucksack
            |> Result.sequence

    let solve input =
        input
        |> Parsing.parseInput
        |> Result.bind Domain.solve

module Part2 =
    open Common.Domain

    module Domain =

        type ElfGroup = ElfGroup of Rucksack list

        let rucksackItemTypes (Rucksack(Compartment c1, Compartment c2)) =
            Set.union c1 c2

        let elfGroupCommonItemType (ElfGroup rucksacks) =
            rucksacks
            |> Seq.map rucksackItemTypes
            |> Set.intersectMany
            |> Seq.tryExactlyOne
            |> Result.ofOption "Elf group doesn't have exactly one common item between its members"

        let solve (input: ElfGroup list) : Result<int, string> =
            Result.result {
                let! itemTypes =
                    input
                    |> List.map elfGroupCommonItemType
                    |> Result.sequence
                return Seq.sumBy itemTypePriority itemTypes
            }

    module Parsing =
        open Domain

        let parseInput (input: string list) : Result<ElfGroup list, string> =
            Result.result {
                let! rucksacks = Part1.Parsing.parseInput input
                return
                    rucksacks
                    |> List.chunkBySize 3
                    |> List.map ElfGroup
            }

    let solve input =
        input
        |> Parsing.parseInput
        |> Result.bind Domain.solve
