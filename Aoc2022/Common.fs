[<AutoOpen>]
module Common

open System.IO

let load (value: string) =
    value.Split('\n')
    |> List.ofArray

let loadInput name =
    let path = Path.Combine(__SOURCE_DIRECTORY__, "inputs", name + ".txt")
    File.ReadAllLines path
    |> List.ofArray
