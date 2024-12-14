module day03

open System.Text.RegularExpressions

let mulRegEx = new Regex("mul\((\d+),(\d+)\)", RegexOptions.Compiled)

let multiplyGroups (groups: Group seq) =
    groups
    |> Seq.skip 1 
    |> Seq.map (
        fun (v) -> 
            System.Convert.ToInt32(v.Value)
        ) 
    |> Seq.fold(fun acc nbr -> acc * nbr) 1

let getSimpleMultiples (fileData: string) =     
    mulRegEx.Matches(fileData)
    |> Seq.map (
        fun (m: Match) -> 
            multiplyGroups m.Groups 
    )
    |> Seq.sum    

    
let complexRegEx = new Regex("do\(\)|don't\(\)|mul\((\d+),(\d+)\)", RegexOptions.Compiled)

let getComplexResults_Regex (fileData: string) =
    let matches = complexRegEx.Matches(fileData) |> Seq.cast<Match>

    let mather (acc: int * bool) (m: Match) =
        let (total, doMath) = acc
        match m.Value with
        | v when v = "do()" -> (total, true)
        | v when v = "don't()" -> (total, false)
        | _ when not doMath -> acc
        | _ -> (total + (multiplyGroups m.Groups), doMath)

    matches |> Seq.fold mather (0, true)


let getCompexResults_Brute (fileData: string) = 
    let charData = fileData.ToCharArray()

    let isMulLength = 4
    let isDoLength = 4
    let isDoNotLength = 7

    let rec findCharacter chr idx = 
        match idx < charData.Length with
        | true -> 
            match charData.[idx] = chr with
            | false -> findCharacter chr (idx + 1)
            | true -> idx
        | _ -> -1


    let isTextMatch idx length text =
        idx + (length - 1) < charData.Length && fileData.Substring(idx, length) = text
        

    let rec parseKeyWords idx output =
        match idx < charData.Length with
        | true -> 
            match charData.[idx] with
            | c when c = 'd' && (isTextMatch idx isDoLength "do()") -> parseKeyWords (idx + isDoLength) ("+"::output)
            | c when c = 'd' && (isTextMatch idx isDoNotLength "don't()") -> parseKeyWords (idx + isDoNotLength) ("-"::output)
            | c when c = 'm' && (isTextMatch idx isMulLength "mul(") -> 
                match findCharacter ')' (idx + isMulLength) with
                | x when x > 0 -> 
                    let testMulValue = fileData.Substring(idx, x - idx + 1)
                    match mulRegEx.IsMatch(testMulValue) with
                    | true -> parseKeyWords (idx + (x - idx + 1)) (testMulValue::output)
                    | false -> parseKeyWords (idx + isMulLength) output
                | _ -> parseKeyWords (idx + isMulLength) output
            | _ -> parseKeyWords (idx + 1) output
        | false -> output


    let rec calculateTotal records doMath finalValue = 
        match records with 
        | head::tail -> 
            match head, doMath with 
            | c, _ when c = "+" -> calculateTotal tail true finalValue
            | c, _ when c = "-" -> calculateTotal tail false finalValue
            | _, true -> calculateTotal tail true  (finalValue + getSimpleMultiples(head))
            | _, false -> calculateTotal tail doMath finalValue
        | [] -> finalValue

    let mathSeq = parseKeyWords 0 [] |> List.rev

    calculateTotal mathSeq true 0


let getResult() =
    let fileData = System.IO.File.ReadAllText("Day03/day03_input.txt") 
    (getSimpleMultiples fileData, getCompexResults_Brute fileData, getComplexResults_Regex fileData)