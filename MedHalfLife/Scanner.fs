module Scanner

open GeneralTypes
open Concentration
open Help
open System

type Scanner() =

    //https://www.mayoclinicproceedings.org/article/S0025-6196(11)64618-1/fulltext
    static let normalHalfLife = 1.5 // 1.5 to 2.5
    static let normalTMax = 3.0 // 1.0 to 3.0

    static let normalDose = 
        {origin=Absolute DateTime.MinValue; amount=20.0; t_max=normalTMax; half_life=normalHalfLife}

    static let (|Dosing|MeasuredTime|Problem|) = function
        | Dose _ -> Dosing
        | TimeAt _ -> MeasuredTime
        | Nothing -> Problem

    static let parseDose (input : string) =
        let input' = input.Split('=') |> Array.toList
        match input' with
        | [] -> Nothing
        | [t;a] ->
            let amount = a|> System.Double.TryParse 
            match amount with
            | (false, _) -> Nothing
            | (true, a') -> 
                {normalDose with origin=Concentration.Time(t); amount=a'} |> Dose
        | [t] -> Concentration.Time(t) |> TimeAt
        | _ -> Nothing

    static member Interpret (input : string) (doses : Dose option list) =
        let input' = input.Split(' ') |> Array.toList
        match input' with
        | [] | [""] -> Exit
        | ["test"] -> Test []
        | "test"::rest -> 
            rest
            |> List.map parseDose
            |> Test 
        | ["add"] -> Error "you must enter time=dose_amount to add; see help"
        | "add"::rest ->
            rest
            |> List.map parseDose
            |> Add
        | ["remove"] -> Error "you must enter time to remove; see help"
        | "remove"::rest ->
            rest
            |> List.map parseDose
            |> Remove
        | ["whenmax"] -> WhenMax []
        | "whenmax"::rest ->
            rest
            |> List.map parseDose
            |> WhenMax
        | ["list"] ->  doses |> Concentration.ToString  |> Show
        | ["reset"] -> Reset
        | ["help"] -> Commands.Help ""
        | ["help"; h] -> Commands.Help h
        | ["example"] -> Error "you must enter command get get example; see help"
        | ["example"; e] -> Example e
        | _ -> Error "enter a valid command"

    static member Start (doses : Dose option list) =
        let rec looper doses' = seq {
            let input = Console.ReadLine()
            let interpretedInput = Scanner.Interpret input doses'
            match interpretedInput with
            | Exit -> yield doses'
            | Error s -> printfn "%A" s; yield! looper doses'
            | Reset -> printfn "Reset complete"; yield! looper []
            | Show s -> printfn "%A" s; yield! looper doses'
            | Help h -> Help.Show h; yield! looper doses'
            | Example e -> Help.Example e; yield! looper doses'
            | Remove d -> 
                let exclude = 
                    d
                    |> List.map (fun x -> 
                        match x with 
                        | Dose y -> Some y 
                        | TimeAt y -> Some {normalDose with origin=y} 
                        | Nothing -> None)
                yield! looper (List.except exclude doses')
            | Add d -> 
                let addition =
                    d
                    |> List.map (function Dose y -> Some y | _ -> None)
                yield! looper (doses'@addition)
            | Test d ->
                let doses'' = 
                    d
                    |> List.map (function Dose y -> Some y | _ -> None)
                    |> (fun x -> doses'@x)
                let times =
                    d
                    |> List.map (function TimeAt y -> Some y | _ -> None)
                    |> List.choose (id)
                match times with
                | [] -> System.Console.WriteLine("{0:0.##0}", (Concentration.At (Absolute DateTime.Now)) doses''); yield! looper doses'
                | t -> 
                    let results = t |> List.map (fun x -> Concentration.At x doses'')
                    results
                    |> List.iter (fun x-> System.Console.WriteLine("{0:0.##0}", x))
                    yield! looper doses'
            | WhenMax d -> 
                let doses'' = 
                    d
                    |> List.map (function Dose y -> Some y | _ -> None)
                    |> (fun x -> doses'@x)
                doses'' |> Concentration.WhenMax |> printfn "%A"; yield! looper doses'
        }
        looper doses

