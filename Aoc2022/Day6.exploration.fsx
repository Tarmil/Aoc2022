#load "Common.fs"

let part1 (input: string) =
    input
    |> Seq.mapi (fun i c -> i + 1, c)
    |> Seq.windowed 4
    |> Seq.pick (fun [| _, a; _, b; _, c; i, d |] ->
        if Set [a; b; c; d] |> Set.count = 4 then
            Some i
        else
            None)

part1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
part1 "bvwbjplbgvbhsrlpgdmjqwftvncz"
part1 "nppdvjthqldpwncqszvftbrmjlhg"
part1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
part1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

part1 (loadInputAsOneString "day6")

let solveFor markerSize (input: string) =
    input
    |> Seq.mapi (fun i c -> i + 1, c)
    |> Seq.windowed markerSize
    |> Seq.pick (fun xs ->
        if Set [ for _, x in xs -> x ] |> Set.count = markerSize then
            Some (fst (Array.last xs))
        else
            None)

let part2 = solveFor 14

part2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
part2 "bvwbjplbgvbhsrlpgdmjqwftvncz"
part2 "nppdvjthqldpwncqszvftbrmjlhg"
part2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
part2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

part2 (loadInputAsOneString "day6")
