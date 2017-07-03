
open System
type Time = 
    | Absolute of DateTime
    | Relative of (DateTime -> DateTime)
type Dose = {origin:Time; amount:float; half_life:float}

type Concentration() =

    static let calculate (current_time : Time) (dose : Dose option) =
        let t = function | Absolute d -> d | Relative d -> DateTime.MinValue
        match dose with 
        | None -> 0.0
        | Some d ->
            if t current_time < DateTime.MinValue ||
                t d.origin > t current_time ||
                d.half_life <= 0.0 ||
                d.amount < 0.0 then 0.0
            else d.amount * (0.5)**(((t current_time) - (t d.origin)).TotalHours/d.half_life)

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

let dose1 = Some {origin=Concentration.Time("9:30"); amount=15.0; half_life=2.0}
let dose2 = Some {origin=Concentration.Time("12:24"); amount=10.0; half_life=2.0}
let dose3 = Some {origin=Concentration.Time("15:38"); amount=10.0; half_life=2.0}
let dose4 = None // Some {origin=Concentration.Time("18:10"); amount=10.0; half_life=2.0}
let dose5 = None // Some {origin=Concentration.Time("20:24"); amount=5.0; half_life=2.0}
let doses : Dose option list = [dose1; dose2; dose3; dose4; dose5]

let current = Concentration.At (Absolute DateTime.Now) doses

System.Console.WriteLine("{0:0.##0}", Concentration.At (Absolute DateTime.Now) doses)
//System.Console.ReadKey() |> ignore