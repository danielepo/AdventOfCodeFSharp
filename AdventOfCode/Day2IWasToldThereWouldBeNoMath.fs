module Day2IWasToldThereWouldBeNoMath
open System.IO
[<Measure>] type feet
let (>>) f g x = g ( f(x) )


type Dimensions =
    { L:int<feet>; W:int<feet>; H:int<feet>}

let dimensions = 
    let input =seq {
        use stream = new  StreamReader(@"D:\Users\EUL1363\Documents\Visual Studio 2013\Projects\AdventOfCode\AdventOfCode\day2Input.txt")
        while not stream.EndOfStream do
            yield stream.ReadLine()
        } 
    input
    |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
    |> Seq.map(fun x ->
        let splitted = 
            x.Trim().Split('x') 
            |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
            |> Seq.map (System.Int32.Parse >> (*) 1<feet> )|> List.ofSeq
        {L= splitted.[0] ;W=splitted.[1] ;H=splitted.[2] })

let getTotalArea = 
    let getArea (dim:Dimensions) = 
        let areaLW = dim.L * dim.W
        let areaWH = dim.W * dim.H
        let areaLH = dim.L * dim.H
        let minArea = min areaLW (min areaWH areaLH)
        2 * areaLW + 2 * areaWH + 2 * areaLH + minArea
    let getRibonLenth (dim:Dimensions) =
        let perim = 
            [dim.L;dim.H;dim.W] 
            |> List.sort 
            |> fun x -> 2 * x.[0] + 2 * x.[1]
        let quad = 
            [int dim.L;int dim.H;int dim.W] 
            |> List.fold (*) 1
            |> (*) 1<feet>
        perim + quad
        
    let paperArea = 
        dimensions 
        |> Seq.map getArea 
        |> Seq.sum
    let ribbonLength = 
        dimensions
        |> Seq.map getRibonLenth
        |> Seq.sum
    (paperArea, ribbonLength)