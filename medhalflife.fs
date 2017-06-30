
open System
type Time = 
    | Absolute of DateTime
    | Relative of (DateTime -> DateTime)
type Dose = {origin:Time; amount:float; half_life:float}

type Concentration() =

    static let calculate (current_time : Time) (dose : Dose) =
        let t = function | Absolute d -> d | Relative d -> DateTime.MinValue

        if t current_time < DateTime.MinValue ||
            t dose.origin > t current_time ||
            dose.half_life <= 0.0 ||
            dose.amount < 0.0 then 0.0
        else dose.amount * (0.5)**(((t current_time) - (t dose.origin)).TotalHours/dose.half_life)

    static member At (current_time : Time) (doses : Dose list) : float =
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

let dose1 = {origin=Concentration.Time("8:31"); amount=15.0; half_life=2.0}
let dose2 = {origin=Concentration.Time("11:13"); amount=10.0; half_life=2.0}
let dose3 = {origin=Concentration.Time("14:00"); amount=10.0; half_life=2.0}
let dose4 = {origin=Concentration.Time("15:25"); amount=5.0; half_life=2.0}
let doses = [dose1; dose2; dose3; dose4]

System.Console.WriteLine("{0:0.##0}", Concentration.At (Concentration.Time("16:45")) doses)
System.Console.ReadKey() |> ignore