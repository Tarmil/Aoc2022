#load "Common.fs"
#r "nuget: FParsec"
#r "nuget: FSharpx.Extras"

open System.Collections.Generic
open FParsec
open FSharpx.Collections

type Entry =
    | Dir of content: DirContents
    | File of size: int

and DirContents = Dictionary<string, Entry>

type LsOutput =
    | ODir of name: string
    | OFile of size: int * name: string

type Command =
    | Cd of path: string
    | Ls of output: LsOutput list

let parse p s =
    match runParserOnString p () "" s with
    | Success(x, _, _) -> x
    | Failure(e, _, _) -> failwith e

let pcd = skipString "cd " >>. restOfLine true |>> Cd
let pls = skipString "ls" >>. skipNewline
let pprompt = skipString "$ "
let plsOutputDir = skipString "dir " >>. restOfLine true |>> ODir
let plsOutputFile = pint32 .>> skipChar ' ' .>>. restOfLine true |>> OFile
let plsOutput = plsOutputDir <|> plsOutputFile
let plsOutputs = many plsOutput
let plsFull = pls >>. plsOutputs |>> Ls
let pcommand = pprompt >>. (pcd <|> plsFull)
let pinput = many pcommand

let input = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"

parse pinput input

let rec findPath (dir: DirContents) (path: string list) =
    match path with
    | [] -> dir
    | x :: rest ->
        match dir.TryGetValue(x) with
        | true, Dir dir -> findPath dir rest
        | _ -> failwith $"Not found: {x}"

// cdPath = current path as a stack (ie in reverse order)
let rec toEntriesLoop (root: DirContents) (cdPath: string list) (cdContents: DirContents) cmds =
    match cmds with
    | [] -> root
    | Cd ".." :: rest ->
        let cdPath = List.tail cdPath
        toEntriesLoop root cdPath (findPath root (List.rev cdPath)) rest
    | Cd "/" :: rest ->
        toEntriesLoop root [] root rest
    | Cd dir :: rest ->
        let (Dir contents) = cdContents[dir]
        toEntriesLoop root (dir :: cdPath) contents rest
    | Ls lines :: rest ->
        for line in lines do
            match line with
            | ODir name -> name, Dir (Dictionary())
            | OFile (size, name) -> name, File size
            |> cdContents.Add
        toEntriesLoop root cdPath cdContents rest

let toEntries cmds =
    let root = Dictionary()
    toEntriesLoop root [] root cmds

toEntries (parse pinput input)

// Returns (thisDirSize, totalSmallDirSizes)
let rec findSmallDirsLoop (root: DirContents) (totalSmallDirSizes: int) =
    let thisDirSize, totalSmallDirSizes =
        ((0, totalSmallDirSizes), root)
        ||> Seq.fold (fun (thisDirSize, totalSmallDirSizes) (KeyValue(_, entry)) ->
            match entry with
            | File size -> thisDirSize + size, totalSmallDirSizes
            | Dir content ->
                let subdirSize, totalSmallDirSizes = findSmallDirsLoop content totalSmallDirSizes
                thisDirSize + subdirSize, totalSmallDirSizes)
    if thisDirSize <= 100_000 then
        thisDirSize, thisDirSize + totalSmallDirSizes
    else
        thisDirSize, totalSmallDirSizes

let findSmallDirs root = findSmallDirsLoop root 0 |> snd

parse pinput input |> toEntries |> findSmallDirs

loadInputAsOneString "day7" |> parse pinput |> toEntries |> findSmallDirs

let rec allDirSizes (root: DirContents) =
    let fileSizes, subdirSizes =
        root
        |> Seq.map (fun (KeyValue(_, entry)) ->
            match entry with
            | File size -> Choice1Of2 size
            | Dir dir -> Choice2Of2 (allDirSizes dir))
        |> List.ofSeq
        |> List.partitionChoices
    let directSubdirSizes = List.map List.head subdirSizes
    let allNestedSubdirSizes = List.concat subdirSizes
    let thisDirSize = List.sum fileSizes + List.sum directSubdirSizes
    thisDirSize :: allNestedSubdirSizes

parse pinput input |> toEntries |> allDirSizes

let solve2 root =
    let allSizes = allDirSizes root
    let rootSize = List.head allSizes
    let toFree = rootSize - 40000000
    allSizes
    |> List.filter (fun size -> size >= toFree)
    |> List.min

parse pinput input |> toEntries |> solve2
loadInputAsOneString "day7" |> parse pinput |> toEntries |> solve2