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
    Value: Int64
    }
type Direction = Right | Left | Up | Down
type Re = {
    Direction:Direction
    Move: Point -> Point
}

let day3input = 289326

let calculateDistance n =
    let buildSpiral n = 
        let goRight p = {p with X = p.X + 1}
        let goLeft p = {p with X = p.X - 1}
        let goUp p = {p with Y = p.Y + 1}
        let goDown p = {p with Y = p.Y - 1}
    
        let ShouldChange p = 
            p.X = p.Y || 
            p.X < 0 && (-p.X) = p.Y || 
            p.X > 0 && p.Y < 0 && p.X = -p.Y + 1


        let getValue (ps:Point list) p =
            let adiacent p1 = 
                p1.X = p.X + 1 && p1.Y = p.Y ||
                p1.X = p.X + 1 && p1.Y = p.Y + 1||
                p1.X = p.X + 1 && p1.Y = p.Y - 1||
                p1.X = p.X - 1 && p1.Y = p.Y ||
                p1.X = p.X - 1 && p1.Y = p.Y + 1||
                p1.X = p.X - 1 && p1.Y = p.Y - 1||
                p1.X = p.X && p1.Y = p.Y + 1||
                p1.X = p.X && p1.Y = p.Y - 1

            ps 
            |> List.filter adiacent 
            |> List.map (fun p1 -> p1.Value) 
            |> List.sum

        let valuePoint (ps:Point list) p = {
                p with
                    Value = getValue ps p
            }
        let point = {Value= 1L; X = 1; Y = 0}
        let points = [point; {Value= 1L; X = 0; Y = 0}]
        let record = {
            Direction = Up
            Move = goUp
        }  
        let rec builder index n point record points=
            if index >= n then point
            else
       
                let newPoint = (record.Move >> valuePoint points) point
            
                let newRecord (record:Re) point = 
                    match ShouldChange point, record.Direction with
                    | true, Right -> 
                        {
                                Direction = Up
                                Move = goUp
                        }
                    | true, Left -> 
                        {
                                Direction = Down
                                Move = goDown
                        }
                    | true, Up -> 
                        {
                                Direction = Left
                                Move = goLeft
                        }
                    | true, Down -> 
                        {
                                Direction = Right
                                Move = goRight
                        }
                    | false, _ -> record  

                let newRec = newRecord record newPoint
                builder (index + 1) n newPoint newRec (newPoint::points)
        builder 2 n point record points

    Seq.initInfinite buildSpiral
    |> Seq.filter (fun x-> x.Value >= n)
    |> Seq.head


(*
calculateDistance 289326L
*)