module Day5DoesntHeHaveInternElvesForThis
open Utils

let stringToCharList (input:string) =
    input |> List.ofSeq

let excluded = function
    | ('a','b') | ('c','d') | ('p','q') | ('x','y') -> true
    | _ -> false

let twice (last,curr) = last = curr

let isVowl = function
    | 'a' | 'e' | 'i' | 'o' | 'u' -> true
    | _ -> false

type Property = {
    NumberVowls: int;
    HasTwice : bool;
    HasExcluded: bool;
    }

let isGood (property:Property) = 
    not property.HasExcluded &&
    property.HasTwice &&
    property.NumberVowls >= 3

let isWordGood word =
    let rec isWordGoodW (word:char list) (lastChar:char) (property:Property) =
        match word with
        | [] -> isGood property
        | x::xs -> isWordGoodW xs x {
            NumberVowls = (if isVowl x then property.NumberVowls + 1 else property.NumberVowls) ; 
            HasTwice = property.HasTwice || twice (lastChar,x); 
            HasExcluded = property.HasExcluded || excluded (lastChar,x)}
    isWordGoodW (List.ofSeq word) ' ' {NumberVowls = 0; HasTwice = false; HasExcluded = false}

let countGoods = 
    "C:\Users\d.pozzobon\GitHubRepos\AdventOfCode\AdventOfCode\Day5Input.txt"
    |> TextFileReader
    |> Seq.map isWordGood
    |> Seq.filter (fun x -> x)
    |> Seq.length