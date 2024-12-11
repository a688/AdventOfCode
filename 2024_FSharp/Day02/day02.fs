module day02

/// <summary>
/// Calculates the distance between two points in a 'report' so we can determine 
//  if they are "safe" or not
/// </summary>
/// <param name="reports"></param>
let appendDistances (report: int array) =
    let distances = 
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
    
    (report, distances)

/// <summary>
/// Validates a report to determine if it is safe or not
/// </summary>
/// <param name="values"></param>
/// <param name="distances"></param>
let appendPassFail ((values: int[]), (distances: int[])) =
    let passOrFail =
        match distances.[0] with
        | x when x > 0 -> distances |> Array.forall(fun y -> y = 1)
        | x when x < 0 -> distances |> Array.forall(fun y -> y = -1)
        | _ -> false
    (values, distances, passOrFail)


let getResult() =
    let processedReports =
        System.IO.File.ReadAllLines("Day02/day02_input.txt") 
        |> Array.map(
            fun (line) -> 
                line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.map int 
            )
        |> Array.map appendDistances
        |> Array.map appendPassFail

    let passInitialFilter =
        processedReports
        |> Array.filter (fun (values, distances, passed) -> passed)

    let retry =
        processedReports
        |> Array.filter (fun (values, distances, passed) -> not passed)
        |> Array.filter (fun (values, distances, passed) -> 
            let justZero = distances |> Array.filter(fun d -> d = 0) |> Array.length
            let tooNegative = distances |> Array.filter(fun d -> d < -1) |> Array.length
            let tooPositive = distances |> Array.filter(fun d -> d > 1) |> Array.length

            (justZero + tooNegative + tooPositive) = 1
        )
        

    // 306
    (passInitialFilter.Length, retry.Length)