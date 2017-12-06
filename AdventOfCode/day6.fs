//#I "."
module Day6
open System

let testInput = [0; 2; 7; 0]
let actualInput = [2; 8; 8; 5; 4; 2; 3; 1; 5; 5; 1; 2; 15; 13; 5; 14]
let valueOr s d = 
    match s with
    | Some v -> v
    | None -> d
// let input = actualInput |> Array.ofList
// let input = testInput |> Array.ofList
// countCycles input
let countCycles (input:int[])= 
    
    let rec cycle (s:Map<string,int>) c =
        let isSecondOccurrence (s:Map<string,int>) (input:int[]) count= 
            let compact = input |> Array.map string |> String.Concat
            if s.ContainsKey compact then 
                let item = s.Item compact
                true, Some (count - item), s
            else
                false, None, s.Add(compact, count)

        
        let findMax input = 
            let max = input |> Array.max
            input, max, input |> Array.findIndex (fun x -> x = max)

        let cleanFirstMax (input:int[], max, index)= 
            input.[index] <- 0
            input, max, index


        let rec redistribute (input:int[], max, index) =
            if max = 0 then ()
            else 
                let nextIndex = if index >= input.Length - 1 then 0 else index + 1
                input.[nextIndex] <- input.[nextIndex] + 1
                redistribute (input, (max - 1), nextIndex)
        
        match isSecondOccurrence s input c with
        | false, _, set -> 
            let count = c + 1 
            input 
            |> findMax 
            |> cleanFirstMax 
            |> redistribute
            cycle set count
        | true, distance,_ -> valueOr distance 0
    
    cycle Map.empty 0