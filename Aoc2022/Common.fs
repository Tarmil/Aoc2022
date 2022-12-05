[<AutoOpen>]
module Common

open System.IO

let load (value: string) =
    value.Split('\n')
    |> List.ofArray

let loadInput name =
    Path.Combine(__SOURCE_DIRECTORY__, "inputs", name + ".txt")
    |> File.ReadAllLines
    |> List.ofArray

let loadInputAsOneString name =
    Path.Combine(__SOURCE_DIRECTORY__, "inputs", name + ".txt")
    |> File.ReadAllText
