module day01

let parseLines (lines: string[]) = 
        let (left, right) = 
                (([], []), lines)
                ||> Array.fold(fun (left, right) line ->
                    match line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) with
                    | [| l; r|] -> (int l) :: left, (int r) :: right
                    | _ -> failwith "bad line"
                )

        (left |> List.sort , right |> List.sort)

let getResult() = 
    let allLines = System.IO.File.ReadAllLines("Day01/day01_input.txt");
    let (sortedL, sortedR) = parseLines allLines
    
    let totalDistance = 
        List.zip sortedL sortedR 
        |> List.sumBy(fun (l, r) -> abs(l - r))

    let nbrCounts = 
            sortedR
            |> List.countBy(fun x -> x)
            |> dict

    let totalSimilarity = 
        sortedL
        |> List.map(fun (v) -> 
            match nbrCounts.TryGetValue v with
            | true, count -> v * count
            | _ -> 0
        )
        |> List.sum

    (totalDistance, totalSimilarity)