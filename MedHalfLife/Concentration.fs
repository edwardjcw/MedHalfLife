module Concentration

open System.Text.Json
open GeneralTypes
open System
open System.IO
open Newtonsoft.Json

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
                d.amount * 0.5 **(((t current_time) - c_max).TotalHours/d.half_life)
            else d.amount * ((t current_time) - (t d.origin)).TotalHours/d.t_max

    static member At (current_time : Time) (doses : Dose option list) : float =
        doses
        |> List.fold (fun level dose -> (calculate current_time dose) + level) 0.0
    
    static member WhenMax (doses : Dose option list) : (Time * float) option =
        let t (Absolute d) = d
        let lastDose = if doses.Length > 0 then doses |> List.last else None
        match lastDose with
        | Some d -> 
            let time = (t d.origin).AddHours(d.t_max) |> Absolute
            (time, (Concentration.At time doses)) |> Some
        | None -> None

    static member Time (input : string) : Time =
        match input.Contains(":") with
        | false -> 
            let hours = Double.TryParse(input)
            match hours with
            | false, _ -> Absolute DateTime.MinValue
            | true, h -> Absolute (DateTime.MinValue.AddHours(h))
        | true -> 
            let dateTimeParsed = DateTime.TryParse input 
            match dateTimeParsed with
            | false, _  -> Absolute DateTime.MinValue
            | true, d -> Absolute d       

    static member Load (input : string) : Dose option list =
        if input |> File.Exists then
            input
            |> File.ReadAllText
            |> JsonConvert.DeserializeObject<Dose option list>
        else []

    static member Save (input : string) (doses : Dose option list) =
        let serialized = JsonConvert.SerializeObject(doses)
        File.WriteAllText(input, serialized)

    static member ToString (doses : Dose option list) : string =
        doses
        |> List.choose id
        |> List.map (fun x -> x.ShortString)
        |> String.concat "\n"
