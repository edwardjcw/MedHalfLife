// >medhalflife -load
// >test 15:38=10
// >23.3422
// >add 15:38=10
// >15:38 10 added to file
// >testat 18:12=10 19:10=5 21:10
// >20.324
// >add 18:12=10 19:10=5
// >list
// >12:10=20, 15:38=10, 18:12=10, 19:10=5


open System
open System.IO
open System.Runtime.Serialization

type Time = Absolute of DateTime

[<CustomEquality>]
[<NoComparison>]
type Dose = {origin:Time; amount:float; t_max:float; half_life:float} with
    member this.ShortString = 
        let e (Absolute d) = d.ToShortTimeString()
        e this.origin + this.amount.ToString() 
    override this.GetHashCode() =
        hash this.origin
    override this.Equals(b) =
        match b with
        | :? Dose as d -> this.origin = d.origin
        | _ -> false

type Concentration() =

    static let calculate (current_time : Time) (dose : Dose option) =
        let t (Absolute d) = d
        match dose with 
        | None -> 0.0
        | Some d ->
            if t current_time < DateTime.MinValue ||
                t d.origin > t current_time ||
                d.half_life <= 0.0 ||
                d.amount < 0.0 || d.t_max < 0.0 then 0.0
            else if ((t current_time) - (t d.origin)).TotalHours > d.t_max then
                let c_max = (t d.origin).AddHours(d.t_max)
                d.amount * (0.5)**(((t current_time) - c_max).TotalHours/d.half_life)
            else d.amount * ((t current_time) - (t d.origin)).TotalHours/d.t_max

    static member At (current_time : Time) (doses : Dose option list) : float =
        doses
        |> List.fold (fun level dose -> (calculate current_time dose) + level) 0.0
    
    static member Time (input : string) : Time =
        match input.Contains(":") with
        | false -> 
            let hours = Double.TryParse(input)
            match hours with
            | (false, _) -> Absolute DateTime.MinValue
            | (true, h) -> Absolute (DateTime.MinValue.AddHours(h))
        | true -> 
            let dateTimeParsed = DateTime.TryParse input 
            match dateTimeParsed with
            | (false, _)  -> Absolute DateTime.MinValue
            | (true, d) -> Absolute d       

    static member Load (input : string) : Dose option list =
        if input |> File.Exists then 
            let binFormatter = Formatters.Binary.BinaryFormatter()
            let stream callback (x : byte array)  = 
                use resource = new MemoryStream(x)
                callback resource
            input
            |> File.ReadAllBytes
            |> (stream binFormatter.Deserialize)
            :?> Dose option list
        else []

    static member Save (input : string) (doses : Dose option list) =
        let binFormatter = Formatters.Binary.BinaryFormatter()
        use stream = new MemoryStream()
        binFormatter.Serialize(stream, doses)
        File.WriteAllBytes(input, stream.ToArray())

    static member ToString (doses : Dose option list) : string =
        doses
        |> List.choose (id)
        |> List.map (fun x -> x.ShortString)
        |> String.concat " "

type DoseInput =
    | Dose of Dose
    | TimeAt of Time
    | Nothing

type Commands = 
    | Test of DoseInput list
    | Add of DoseInput list
    | Remove of DoseInput list
    | Show of string
    | Reset 
    | Error of String
    | Exit

type Scanner() =

    static let normalDose = 
        {origin=Absolute DateTime.MinValue; amount=20.0; t_max=2.1; half_life=3.0}

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
        | ["add"] -> Error "you must enter time=dose_amount to add"
        | "add"::rest ->
            rest
            |> List.map parseDose
            |> Add
        | ["remove"] -> Error "you must enter time to remove"
        | "remove"::rest ->
            rest
            |> List.map parseDose
            |> Remove
        | ["list"] ->  doses |> Concentration.ToString  |> Show
        | ["reset"] -> Reset
        | _ -> Error "enter a valid command"

    static member Start (doses : Dose option list) =
        let rec looper doses' = seq {
            let input = Console.ReadLine()
            let interpretedInput = Scanner.Interpret input doses'
            match interpretedInput with
            | Exit -> Concentration.Save "medhalflife.bin" doses'; yield ()
            | Error s -> printfn "%A" s; yield! looper doses'
            | Reset -> printfn "Reset complete"; yield! looper []
            | Show s -> printfn "%A" s; yield! looper doses'
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
        }
        looper doses
        

[<EntryPoint>]
let main argv = 
    match argv.[0].ToLower() with
    | "-load" -> "medhalflife.bin" |> Concentration.Load |> Scanner.Start |> Seq.last |> ignore
    | _ -> [] |> Scanner.Start |> Seq.last |> ignore
    0
