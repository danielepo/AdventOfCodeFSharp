module Day6ProbablyFireHazard

open Utils

let n = [0..999]
let mutable grid = Array.init n.Length (fun y -> Array.init n.Length (fun x -> false))

let input = "toggle 461,550 through 564,900"
let toggle1 = "toggle 0,0 through 0,0"
let toggle2 = "toggle 1,0 through 1,1"

type Rect = 
    { TopLeft : int * int
      BottomRight : int * int }
let Y = function
    | (_,y) -> y
let X = function
    | (x,_) -> x

let (|Instruction|_|) (start) (input : string) = 
    let isToggle = input.StartsWith start
    if not isToggle then None
    else 
        let rect = input.Substring(start.Length + 1).Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofSeq 
        let f = rect.[0]
        let s = rect.[2]
        let toTuple (input : string seq) = 
            input
            |> List.ofSeq
            |> (fun x -> (System.Int32.Parse(x.[0]), System.Int32.Parse(x.[1])))
        let first = f.Split(',') |> toTuple
        let second = s.Split(',') |> toTuple
        Some { TopLeft = first
               BottomRight = second }


let ParseInstruction = 
    let inRange (i,j) (x:Rect) = i >= X x.TopLeft && i <= X x.BottomRight && j >= Y x.TopLeft && j<= Y x.BottomRight
    let toOpp (r:Rect) op = [for y in n -> [for x in n -> if inRange (x,y) r then (op) else ((||) false)]]
    function 
    | Instruction "toggle" r -> toOpp r (not)
    | Instruction "turn on" r -> toOpp r ((||) true)
    | Instruction "turn off" r -> toOpp r ((&&) false)

let Apply (inst: (bool -> bool) list list) =
    for y in n do 
        for x in n do grid.[y].[x] <- inst.[y].[x] grid.[y].[x]

let SumTrue (grid:bool array array)= 
     grid
    |> Array.map (fun x -> x |> Array.filter (fun y -> y) |> Array.length) 
    |> Array.sum


let path1 = @"C:\Users\d.pozzobon\GitHubRepos\AdventOfCode\AdventOfCode\Day6Input.txt"
let path2 = @"C:\Users\d.pozzobon\GitHubRepos\AdventOfCode\AdventOfCode\Day6Input2.txt"
let Run path =
    let rec Lights (instructions: string list) = 
        
        match instructions with 
        | i::is ->
            printf "%s - remaining %d\n" i is.Length
            ParseInstruction i |> Apply
            Lights is 
        | _ -> ()
    
    path
    |> TextFileReader 
    |> List.ofSeq
    |> Lights 
    SumTrue grid