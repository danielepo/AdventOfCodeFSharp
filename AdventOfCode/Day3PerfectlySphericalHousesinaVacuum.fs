module Day3PerfectlySphericalHousesinaVacuum
open Utils

type House =
    {X:int;Y:int}

type Direction =
    | Up
    | Down
    | Left
    | Right

type Move =
    | Santa of house:House
    | RoboSanta of house:House

let path = @"D:\Users\EUL1363\Documents\Visual Studio 2013\Projects\AdventOfCode\AdventOfCode\day3Input.txt"


let grid = [{X=0;Y=0}]
let moves = [Santa grid.[0];RoboSanta grid.[0]]

let getDirectionFromChar = function
    | '^' -> Up
    | 'v' -> Down
    | '<' -> Left
    | '>' -> Right

let visitedHouse direction (lastHouse:House) = 
    match direction with
    | Up -> {X = lastHouse.X;Y = lastHouse.Y + 1}
    | Down -> {X = lastHouse.X; Y = lastHouse.Y - 1}
    | Left -> {X = lastHouse.X - 1 ; Y = lastHouse.Y}
    | Right -> {X = lastHouse.X + 1; Y = lastHouse.Y }

let VisitHouses (directions:string) = 
    let getHouse (move:Move) =
        match move with
        | Santa h -> h
        | RoboSanta h -> h

    let rec vhw position (grid:list<House>) (moves:list<Move>)=
        if directions.Length = 0 || position >= directions.Length 
        then grid.Length
        else 
            let direction = getDirectionFromChar (directions.[position])
            let vMoves =
                let vHouse = moves |> List.tail |> List.head |> getHouse |> visitedHouse direction 
                match List.head moves with
                | Santa lastHouse -> RoboSanta vHouse :: moves
                | RoboSanta lastHouse -> Santa vHouse :: moves
            let newHouse = vMoves |> List.head |> getHouse
            if List.exists (fun elm -> elm = newHouse) grid 
            then vhw (position + 1) grid vMoves
            else vhw (position + 1) (newHouse::grid) vMoves
    vhw 0 grid moves

let count = 
    TextFileReader path 
    |> Seq.head
    |> VisitHouses
    