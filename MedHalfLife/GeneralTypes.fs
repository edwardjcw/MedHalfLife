module GeneralTypes

open System
open System.IO
open System.Runtime.Serialization

type Time = Absolute of DateTime

[<CustomEquality>]
[<NoComparison>]
type Dose = {origin:Time; amount:float; t_max:float; half_life:float} with
    member this.ShortString = 
        let e (Absolute d) = d.ToString()
        e this.origin + "=" + this.amount.ToString() 
    override this.GetHashCode() =
        hash this.origin
    override this.Equals(b) =
        match b with
        | :? Dose as d -> this.origin = d.origin
        | _ -> false

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