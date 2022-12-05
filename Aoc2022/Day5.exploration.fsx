#load "Common.fs"
#r "nuget: FParsec"
open FParsec

let parse p s =
    match runParserOnString p ()  "" s with
    | Success(x, _, _) -> x
    | Failure(e, pe, _) -> printfn "%A" pe; failwith e

let pcrate = skipChar '[' >>. anyChar .>> skipChar ']' |>> Some
let pnoCrate = skipString "   " >>% None
let pmaybeCrate = pcrate <|> pnoCrate
let pcrateLine = sepBy1 pmaybeCrate (skipChar ' ')

parse pmaybeCrate "   "
parse pmaybeCrate "[D]"
parse pcrateLine "    [D]    "
parse pcrateLine "[A] [D]    "
parse pcrateLine "[A]"

let pcrates = sepEndBy pcrateLine skipNewline

// The '\n\'s are to prevent trailing-whitespace-deletion
let input = "    [D]    \n\
[N] [C]    \n\
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"
parse pcrates input

let pcrates' = pcrates |>> (List.transpose >> List.map (List.choose id))

parse pcrates' input

//let pcrateIndex = skipMany (skipChar ' ') >>. pint32
//let pcrateIndexLine = many pcrateIndex |>> List.last
// Nevermind. since the crate stacks contain the correct trailing whitespace,
// we're parsing lists of the same length, so we can just do List.transpose,
// no need to parse the number of crates.

let pactionLine =
    pipe3 (skipString "move " >>. pint32)
          (skipString " from " >>. pint32)
          (skipString " to " >>. pint32)
          (fun move from to' -> {| move = move; from = from - 1; to' = to' - 1 |})

let pactions = sepEndBy pactionLine skipNewline

parse pactions "move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"

let pinput =
    pipe4 pcrates'
          (skipManyTill skipAnyChar skipNewline) // Stack numbers
          skipNewline // empty line
          pactions
          (fun crates () () actions -> {| crates = crates; actions = actions |})

let parseInput = parse pinput

parseInput input

let move count fromStack toStack =
    let toMove, restFromStack = List.splitAt count fromStack
    restFromStack, List.rev toMove @ toStack

let part1 input =
    let input = parseInput input
    let stacks = Array.ofList input.crates
    for action in input.actions do
        let newFrom, newTo = move action.move stacks[action.from] stacks[action.to']
        stacks[action.from] <- newFrom
        stacks[action.to'] <- newTo
    stacks
    |> Array.map List.head
    |> System.String

part1 input

part1 <| loadInputAsOneString "day5"

let move2 count fromStack toStack =
    let toMove, restFromStack = List.splitAt count fromStack
    restFromStack, toMove @ toStack

let part2 input =
    let input = parseInput input
    let stacks = Array.ofList input.crates
    for action in input.actions do
        let newFrom, newTo = move2 action.move stacks[action.from] stacks[action.to']
        stacks[action.from] <- newFrom
        stacks[action.to'] <- newTo
    stacks
    |> Array.map List.head
    |> System.String

part2 input

part2 <| loadInputAsOneString "day5"
