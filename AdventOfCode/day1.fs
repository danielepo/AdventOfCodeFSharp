//open System.Runtime.Remoting.Metadata.W3cXsd2001
//#I "."
module Day1
open System
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

