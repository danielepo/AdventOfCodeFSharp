//open System.Runtime.Remoting.Metadata.W3cXsd2001
//#I "."
module Day3
open System
open System.IO

let calculateChecksum (findTwoNumbers:int [] -> int*int) (reduce: int*int -> int) (rows:string list) = 
    rows 
    |> List.map (fun x ->x.Split('\t') |> Array.map System.Int32.Parse)
    |> List.map (findTwoNumbers >> reduce)
    |> List.sum

let day2input = File.ReadAllLines "./day2input.txt" |> List.ofArray

calculateChecksum (fun x -> Array.min x, Array.max x) (fun (min,max) -> max - min) day2input |>
printf "%d\n" 

let findTwoDivisibles (l:int []) =
    let numbers = l |> List.ofArray |> List.sort
    let rec divisibles n1 n2 =
        match n1,n2 with
        | x1::_, x2::xs2 -> 
            if x1 = x2 then  divisibles n1 xs2
            elif x1 % x2 = 0 then x1, x2
            elif x2 % x1 = 0 then x2, x1
            else divisibles n1 xs2
        | _::xs1, [] -> divisibles xs1 numbers
        | [],_ -> 0, 0 
    divisibles numbers numbers

let divideThem (a,b) = a / b
calculateChecksum findTwoDivisibles divideThem day2input |>
printf "%d\n" 
