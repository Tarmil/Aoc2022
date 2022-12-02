#load "Common.fs"

let parseInput (input: string list) : int list list =
    let rec loop current input =
        match input with
        | [] -> [ List.rev current ]
        | "" :: rest -> List.rev current :: loop [] rest
        | item :: rest -> loop (int item :: current) rest

    loop [] input

let part1 (input: int list list) : int =
    input
    |> List.map List.sum
    |> List.max

let example = load "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"

example |> parseInput |> part1

loadInput "day1" |> parseInput |> part1

let part2 (input: int list list) : int =
    input
    |> List.map List.sum
    |> List.sortDescending
    |> List.take 3
    |> List.sum

example |> parseInput |> part2

loadInput "day1" |> parseInput |> part2
