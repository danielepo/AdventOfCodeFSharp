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
    HasTwice : bool
    }