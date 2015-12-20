module Day5DoesntHeHaveInternElvesForThis
open Utils
let (>>) f g x = g ( f(x) )
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


    

let hasRepeated (input:string) = 
    let rec repeats (sil:string) (remaining:string) (shiftedInput:string)=
        if remaining.Length <= 1 then false
        else 
            if remaining.Contains sil then true
            else if shiftedInput.Length < 2 then false else  
                let newSil = shiftedInput.Substring(0,2)
                repeats newSil (shiftedInput.Substring(2)) (shiftedInput.Substring(1))
    if input.Length < 2 then false else
    repeats (input.Substring(0,2)) (input.Substring(2)) (input.Substring(1)) 


let hasRepeatedWithOneLetter (input:string) =
    let rec repeats (remaining:string) =
        if remaining.Length <= 2 then false
        else 
            let sil = remaining.Substring(0,3) |> List.ofSeq
            if sil.[0] = sil.[2] then true else
            repeats (remaining.Substring(1))
    if input.Length < 2 then false else
    repeats input

let countGoods2 = 
    "C:\Users\d.pozzobon\GitHubRepos\AdventOfCode\AdventOfCode\Day5Input.txt"
    |> TextFileReader
    |> Seq.map (fun x -> hasRepeated x && hasRepeatedWithOneLetter x)
    |> Seq.filter (fun x -> x)
    |> Seq.length