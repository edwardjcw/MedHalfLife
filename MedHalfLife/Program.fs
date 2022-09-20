// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Concentration
open Help
open Scanner


[<EntryPoint>]
let main argv = 
    let fullProcess input = 
        input
        |> Concentration.Load 
        |> Scanner.Start 
        |> Seq.last 
        |> Concentration.Save input       
    match argv with
    | [|"-?"|] -> Help.Show ""
    | [|"-load"|] -> "medhalflife.bin" |> fullProcess
    | [|"-load"; n|] -> n |> fullProcess
    | _ -> [] |> Scanner.Start |> Seq.last |> ignore
    0
