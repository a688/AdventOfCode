module day02

let getDistances (child: int array) =
    child 
    |> Array.pairwise 
    |> Array.map (
        fun (a, b) -> a - b
    )

let sameDirectionAndNoZeros (arr: int array) =
    match arr.[0] with
    | x when x > 0 -> arr |> Array.forall(fun y -> y > 0)
    | x when x < 0 -> arr |> Array.forall(fun y -> y < 0)
    | _ -> false

let limitedDistance (arr: int array) =
        arr
        |> Array.forall(fun x -> x > -4 && x < 4)

let getResult() =
    let passFilters =
        System.IO.File.ReadAllLines("Day02/day02_input.txt") 
        |> Array.map(
            fun (line) -> 
                line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.map int 
            )
        |> Array.map getDistances
        |> Array.filter sameDirectionAndNoZeros
        |> Array.filter limitedDistance

    passFilters.Length