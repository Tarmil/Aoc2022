module Aoc2022.Day7

open System.Collections.Generic
open FSharpx
open FSharpx.Collections

module Common =

    module Domain =

        type Entry =
            | Dir of contents: DirContents
            | File of size: int

        and DirContents = Dictionary<string, Entry>

        [<RequireQualifiedAccess>]
        type LsOutput =
            | Dir of name: string
            | File of size: int * name: string

        type Command =
            | Cd of path: string
            | Ls of output: LsOutput list

        let commandsToEntries (commands: Command list) : Result<DirContents, string> =

            let rec findPath (dir: DirContents) (path: string list) =
                match path with
                | [] -> Ok dir
                | x :: rest ->
                    match dir.TryGetValue(x) with
                    | true, Dir dir -> findPath dir rest
                    | _ -> Error $"Not found: {x}"

            let rec loop (root: DirContents) (cdPath: string list) (cdContents: DirContents) cmds =
                match cmds with
                | [] -> Ok root
                | Cd ".." :: rest ->
                    match cdPath with
                    | [] -> Error "cd .. from the root"
                    | _ :: parentPath ->
                        Result.result {
                            let! parentContents = findPath root (List.rev parentPath)
                            return! loop root parentPath parentContents rest
                        }
                | Cd "/" :: rest ->
                    loop root [] root rest
                | Cd dir :: rest ->
                    match cdContents.TryGetValue(dir) with
                    | false, _ -> Error $"Directory not found: {dir}"
                    | true, File _ -> Error $"Trying to cd into a file: {dir}"
                    | true, Dir contents -> loop root (dir :: cdPath) contents rest
                | Ls lines :: rest ->
                    lines
                    |> List.iter (fun line ->
                        let name, entry =
                            match line with
                            | LsOutput.Dir name -> name, Dir (Dictionary())
                            | LsOutput.File(size, name) -> name, File size
                        cdContents.Add(name, entry))
                    loop root cdPath cdContents rest

            let root = Dictionary()
            loop root [] root commands

        type DirSizes =
            { thisDirSize: int
              subdirSizes: int list }

        let rec allDirSizes (root: DirContents) =
            let fileSizes, subdirSizes =
                root
                |> Seq.map (fun (KeyValue(_, entry)) ->
                    match entry with
                    | File size -> Choice1Of2 size
                    | Dir dir -> Choice2Of2 (allDirSizes dir))
                |> List.ofSeq
                |> List.partitionChoices

            let directSubdirSizes = subdirSizes |> List.map (fun s -> s.thisDirSize)
            let thisDirSize = List.sum fileSizes + List.sum directSubdirSizes

            let allSubdirSizes = subdirSizes |> List.collect (fun s -> s.thisDirSize :: s.subdirSizes)

            { thisDirSize = thisDirSize
              subdirSizes = allSubdirSizes }

    module Parsing =
        open Domain
        open FParsec

        let pcd = skipString "cd " >>. restOfLine true |>> Cd

        let pls = skipString "ls" >>. skipNewline

        let pprompt = skipString "$ "

        let plsOutputDir = skipString "dir " >>. restOfLine true |>> LsOutput.Dir

        let plsOutputFile = pint32 .>> skipChar ' ' .>>. restOfLine true |>> LsOutput.File

        let plsOutput = plsOutputDir <|> plsOutputFile

        let plsOutputs = many plsOutput

        let plsFull = pls >>. plsOutputs |>> Ls

        let pcommand = pprompt >>. (pcd <|> plsFull)

        let pinput = many pcommand

        let parseInput input =
            match runParserOnString pinput () "" input with
            | Success (x, _, _) -> Result.Ok x
            | Failure (e, _, _) -> Result.Error e

module Part1 =

    module Domain =
        open Common.Domain

        let solve (root: DirContents) =
            let rootDirSizes = allDirSizes root
            rootDirSizes.thisDirSize :: rootDirSizes.subdirSizes
            |> List.filter (fun s -> s <= 100_000)
            |> List.sum

    let solve input =
        Result.result {
            let! commands = Common.Parsing.parseInput input
            let! root = Common.Domain.commandsToEntries commands
            return Domain.solve root
        }

module Part2 =

    module Domain =
        open Common.Domain

        let solve (root: DirContents) =
            let rootDirSizes = allDirSizes root
            let toFree = rootDirSizes.thisDirSize - 40_000_000
            rootDirSizes.subdirSizes
            |> List.filter (fun s -> s >= toFree)
            |> List.min

    let solve input =
        Result.result {
            let! commands = Common.Parsing.parseInput input
            let! root = Common.Domain.commandsToEntries commands
            return Domain.solve root
        }
