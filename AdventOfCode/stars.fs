//open System.Runtime.Remoting.Metadata.W3cXsd2001
//#I "."
module Stars
open System
open System.IO
let calculateCapcha (input:string) =
    let toIntList (input:string) =
        input.ToCharArray()
        |> Array.map (fun x -> Int32.Parse(x.ToString()))
        |> List.ofArray

    let rec circularize l = seq{
        yield! l
        yield! circularize l
        }

    let scroll (lista1:int list) =
        let lista2 = 
            lista1 
            |> circularize
            |> Seq.skip (lista1.Length / 2)

        Seq.zip (lista1 |> Seq.ofList) lista2
        |> Seq.map (fun (x1,x2) -> if x1 = x2 then x1 else 0) 
        |> List.ofSeq
        
    input  
    |> (toIntList >> scroll)
    |> List.sum


   
    
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

type Point = {
    X: int
    Y: int
    }
let buildSpiral n = 
    let mutable point = { X=0 ;Y = 0}
    let mutable lengthX = 1
    let mutable lengthY = 1
    let goRight p = {p with X = p.X + 1}
    let goLeft p = {p with X = p.X - 1}
    let goUp p = {p with X = p.Y + 1}
    let goDown p = {p with X = p.Y - 1}
    let mutable move = goRight
    for x = 2 to n do
        let p = move point
        point <- p

