//open System.Runtime.Remoting.Metadata.W3cXsd2001
//#I "."
module Day3
open System

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