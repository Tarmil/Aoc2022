#load "Common.fs"
#r "nuget: FParsec"

open FParsec

type Instruction =
    | Addx of int
    | Noop

let paddx = skipString "addx " >>. pint32 |>> Addx
let pnoop = skipString "noop" >>% Noop
let pinstr = paddx <|> pnoop
let pprogram = sepEndBy pinstr skipNewline
let parseInput input =
    match runParserOnString pprogram () "" input with
    | Success(x, _, _) -> x
    | Failure(e, _, _) -> failwith e

parseInput "addx -1
noop"

let runAndGetSuccessiveX program =
    let _finalX, successiveX =
        ((1, []), program)
        ||> List.fold (fun (x, xs) instr ->
            match instr with
            | Noop -> x, x::xs
            | Addx v -> x + v, x::x::xs)
    List.rev successiveX |> Array.ofList

let part1 program =
    let successiveX = runAndGetSuccessiveX program
    successiveX[19] * 20
    + successiveX[59] * 60
    + successiveX[99] * 100
    + successiveX[139] * 140
    + successiveX[179] * 180
    + successiveX[219] * 220

let sample = loadInputAsOneString "day10.sample" |> parseInput
part1 sample

let input = loadInputAsOneString "day10" |> parseInput
part1 input



let part2 program =
    let successiveX = runAndGetSuccessiveX program
    successiveX
    |> Seq.chunkBySize 40
    |> Seq.map (
        Array.mapi (fun screenPos x ->
            if abs (x - screenPos) <= 1 then
                '#'
            else
                '.')
        >> System.String)
    |> Seq.iter (printfn "%s")

part2 sample
part2 input
