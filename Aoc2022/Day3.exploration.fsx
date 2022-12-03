#load "Common.fs"

type ItemType = ItemType of char

type ItemTypesInCompartment = Set<ItemType>

let isLowPriority (ItemType c) =
    c >= 'a' && c <= 'z'

let isHighPriority (ItemType c) =
    c >= 'A' && c <= 'Z'

let itemType c =
    let itemType = ItemType c
    if isLowPriority itemType || isHighPriority itemType then
        Some itemType
    else
        None

itemType 'b'
itemType 'Z'
itemType '0'

let itemTypePriority (ItemType c as itemType) =
    if isLowPriority itemType then
        int c - int 'a' + 1
    else
        int c - int 'A' + 27

itemTypePriority (ItemType 'c')
itemTypePriority (ItemType 'C')

type ItemTypesInRucksack = ItemTypesInCompartment * ItemTypesInCompartment

let parseRucksack (s: string) : ItemTypesInRucksack =
    let pivot = s.Length / 2
    let firstCompartment = s[..pivot - 1] |> Seq.map ItemType |> Set
    let secondCompartment = s[pivot..] |> Seq.map ItemType |> Set
    ItemTypesInRucksack (firstCompartment, secondCompartment)

let c1, c2 = parseRucksack "vJrwpWtwJgWrhcsFMMfFFhFp"
Seq.map itemTypePriority c1

let commonItemType (c1, c2) =
    Set.intersect c1 c2
    |> Seq.tryExactlyOne

commonItemType (parseRucksack "vJrwpWtwJgWrhcsFMMfFFhFp")

let solve (input: string list) : int =
    input
    |> List.map parseRucksack
    |> List.choose commonItemType
    |> List.sumBy itemTypePriority

solve <| load "vJrwpWtwJgWrhcsFMMfFFhFp"
solve <| load "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"
solve <| loadInput "day3"

type ElfGroup = ElfGroup of ItemTypesInRucksack list

let elfGroupCommonItemType (ElfGroup rucksacks) =
    rucksacks
    |> List.map (fun (c1, c2) -> Set.union c1 c2)
    |> Set.intersectMany
    |> Seq.tryExactlyOne

let solve2 (input: string list) : int =
    input
    |> List.map parseRucksack
    |> List.chunkBySize 3
    |> List.map ElfGroup
    |> List.choose elfGroupCommonItemType
    |> List.sumBy itemTypePriority

solve2 <| load "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"
solve2 <| loadInput "day3"