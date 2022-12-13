#load "Common.fs"
#r "nuget: FParsec"

open FParsec

type Item =
    | Number of int
    | List of Item list

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
    | Success(x, _, _) -> x
    | Failure(e, _, _) -> failwith e

let sample = "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
"
parseInput sample

//type Order = Right | Wrong
//
//let compare' x y =
//    let comp = compare x y
//    if comp = 0 then
//        None
//    elif comp < 0 then
//        Some Right
//    else
//        Some Wrong

let rec compareItems i1 i2 =
    match i1, i2 with
    | Number x1, Number x2 -> compare x1 x2
    | List l1, List l2 -> compareLists l1 l2
    | List l1, (Number _ as x2) -> compareLists l1 [x2]
    | (Number _ as x1), List l2 -> compareLists [x1] l2

and compareLists l1 l2 =
    match l1, l2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x1::rest1, x2::rest2 ->
        match compareItems x1 x2 with
        | 0 -> compareLists rest1 rest2
        | res -> res

sample |> parseInput |> List.map (fun (x, y) -> compareItems x y)

let part1 input =
    input
    |> parseInput
    |> Seq.map (fun (x, y) -> compareItems x y)
    |> Seq.mapi (fun i res ->
        match res with
        | -1 -> i + 1
        | _ -> 0)
    |> Seq.sum

part1 sample
part1 (loadInputAsOneString "day13")

let sep2 = List [List [Number 2]]
let sep6 = List [List [Number 6]]

let sortInput input =
    input
    |> parseInput
    |> List.collect (fun (x, y) -> [x; y])
    |> List.append [sep2; sep6]
    |> List.sortWith compareItems

sortInput sample

let part2 input =
    let l = sortInput input
    let i1 = List.findIndex (fun x -> x = sep2) l
    let i2 = List.findIndex (fun x -> x = sep6) l
    (i1+1) * (i2+1)

part2 sample
part2 (loadInputAsOneString "day13")