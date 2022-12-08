#load "Common.fs"

let parseInput (input: string list) =
    input
    |> Seq.map (Seq.map int >> Array.ofSeq)
    |> Array.ofSeq

let markVisibleFromTheLeft (line: int[]) (marker: bool[]) =
    let rec loop tallestSeen i =
        if i < line.Length then
            if line[i] > tallestSeen then
                marker[i] <- true
                loop line[i] (i+1)
            else
                loop tallestSeen (i+1)
    loop -1 0

let forest = parseInput <| load "\
30373
25512
65332
33549
35390"
for line in forest do
    let marker = Array.create forest.Length false
    markVisibleFromTheLeft line marker
    printfn "%A" marker

let markVisibleFromTheRight (line: int[]) (marker: bool[]) =
    let rec loop tallestSeen i =
        if i >= 0 then
            if line[i] > tallestSeen then
                marker[i] <- true
                loop line[i] (i-1)
            else
                loop tallestSeen (i-1)
    loop -1 (line.Length-1)

for line in forest do
    let marker = Array.create forest.Length false
    markVisibleFromTheRight line marker
    printfn "%A" marker

let markVisibleFromTheTop (forest: int[][]) (marker: bool[][]) (column: int) =
    let rec loop tallestSeen i =
        if i < forest.Length then
            if forest[i][column] > tallestSeen then
                marker[i][column] <- true
                loop (forest[i][column]) (i+1)
            else
                loop tallestSeen (i+1)
    loop -1 0

let marker = Array.init forest.Length (fun _ -> Array.create forest[0].Length false)
for i in 0..forest.Length-1 do
    markVisibleFromTheTop forest marker i
    printfn "%A" marker

let markVisibleFromTheBottom (forest: int[][]) (marker: bool[][]) (column: int) =
    let rec loop tallestSeen i =
        if i >= 0 then
            if forest[i][column] > tallestSeen then
                marker[i][column] <- true
                loop (forest[i][column]) (i-1)
            else
                loop tallestSeen (i-1)
    loop -1 (forest.Length-1)

let marker = Array.init forest.Length (fun _ -> Array.create forest[0].Length false)
for i in 0..forest.Length-1 do
    markVisibleFromTheBottom forest marker i
    printfn "%A" marker

let solve (forest: int[][]) =
    let marker = Array.init forest.Length (fun _ -> Array.create forest[0].Length false)
    for i = 0 to forest.Length-1 do
        markVisibleFromTheTop forest marker i
        markVisibleFromTheBottom forest marker i
    for i = 0 to forest[0].Length-1 do
        markVisibleFromTheLeft forest[i] marker[i]
        markVisibleFromTheRight forest[i] marker[i]
    marker |> Seq.sumBy (Seq.filter id >> Seq.length)

solve forest

loadInput "day8" |> parseInput |> solve

let leftScore (forest: int[][]) (x: int) (y: int) =
    let line = forest[x]
    let rec findLimit i =
        if i = 0 || line[i] >= line[y] then
            i
        else
            findLimit (i-1)
    if y = 0 then 0 else y - findLimit (y-1)

forest |> Array.iteri (fun x l ->
    l |> Array.iteri (fun y _ ->
        printf "%i" (leftScore forest x y))
    printfn "")

let rightScore (forest: int[][]) (x: int) (y: int) =
    let line = forest[x]
    let lineEnd = line.Length - 1
    let rec findLimit i =
        if i = lineEnd || line[i] >= line[y] then
            i
        else
            findLimit (i+1)
    if y = lineEnd then 0 else findLimit (y+1) - y

forest |> Array.iteri (fun x l ->
    l |> Array.iteri (fun y _ ->
        printf "%i" (rightScore forest x y))
    printfn "")

let topScore (forest: int[][]) (x: int) (y: int) =
    let rec findLimit i =
        if i = 0 || forest[i][y] >= forest[x][y] then
            i
        else
            findLimit (i-1)
    if x = 0 then 0 else x - findLimit (x-1)

forest |> Array.iteri (fun x l ->
    l |> Array.iteri (fun y _ ->
        printf "%i" (topScore forest x y))
    printfn "")

let bottomScore (forest: int[][]) (x: int) (y: int) =
    let colEnd = forest.Length-1
    let rec findLimit i =
        if i = colEnd || forest[i][y] >= forest[x][y] then
            i
        else
            findLimit (i+1)
    if x = colEnd then 0 else findLimit (x+1) - x

forest |> Array.iteri (fun x l ->
    l |> Array.iteri (fun y _ ->
        printf "%i" (bottomScore forest x y))
    printfn "")

let part2 (forest: int[][]) =
    Seq.allPairs {0..forest.Length-1} {0..forest[0].Length-1}
    |> Seq.map (fun (x, y) ->
        leftScore forest x y * rightScore forest x y * topScore forest x y * bottomScore forest x y)
    |> Seq.max

part2 forest
loadInput "day8" |> parseInput |> part2
