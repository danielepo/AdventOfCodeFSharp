module Stars
open System

let calculateCapcha (input:string) =
    let toIntList (input:string) =
        input.ToCharArray()
        |> Array.map (fun x -> Int32.Parse(x.ToString()))
        |> List.ofArray

    let scroll (ls:int list) =

        let rec runner first =
            function
            | x1 :: x2 :: xs -> if x1 = x2 then x1 :: (runner first (x2 :: xs)) else (runner first (x2 :: xs))
            | x1 :: [] -> if x1 = first then x1 :: [] else []
            | [] -> []
        
        runner ls.Head ls

    input  
    |> (toIntList >> scroll)
    |> List.sum