module day02

let calculateDistances (report: int array) = 
    report 
    |> Array.pairwise 
    |> Array.map (
        fun (a, b) -> 
            match a - b with
            | x when x > 0 && x < 4 -> 1
            | x when x > 3 -> 2
            | x when x < 0 && x > -4 -> -1
            | x when x < 0 && x < -4 -> -2
            | _ -> 0
    )

/// <summary>
/// Validates a report to determine if it is safe or not
/// </summary>
/// <param name="distances"></param>
let getPassFail (distances: int[]) =
    match distances.[0] with
    | x when x > 0 -> distances |> Array.forall(fun y -> y = 1)
    | x when x < 0 -> distances |> Array.forall(fun y -> y = -1)
    | _ -> false


let canReprocess (report: int array) = 
    let rec generateRemix removeAt currIdx newMix = 
        match currIdx with 
        | x when x = removeAt -> generateRemix removeAt (currIdx + 1) newMix
        | x when x < report.Length -> generateRemix removeAt (currIdx + 1) (report.[currIdx]::newMix)
        | _ -> newMix

    let rec findRemix idx =
        match idx with 
        | x when x < report.Length -> 
            let newMix = generateRemix idx 0 [] |> List.rev |> List.toArray
            match getPassFail (calculateDistances newMix)  with
            | false -> findRemix (idx + 1)
            | true -> true
        | _ -> false
        
    findRemix 0

let getResult() =
    let processedReports =
        System.IO.File.ReadAllLines("Day02/day02_input.txt") 
        |> Array.map(
            fun (line) -> 
                line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.map int 
            )
        |> Array.map (fun r -> 
            let d = calculateDistances r
            (r, d, getPassFail d)
            )

    let passInitialFilter =
        processedReports
        |> Array.filter (fun (_, _, passed) -> passed)

    let retry =
        processedReports
        |> Array.filter (fun (_, _, passed) -> not passed)
        |> Array.filter (fun (v, _, _) -> canReprocess v)
        

    // 306, 60, 366
    (passInitialFilter.Length, retry.Length, passInitialFilter.Length + retry.Length)