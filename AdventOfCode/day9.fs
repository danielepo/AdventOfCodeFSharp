//#I "."
//__SOURCE_DIRECTORY__
//__SOURCE_FILE__
//__LINE__
module Day9
open System
type Group = Group of Group list
type Garbage = unit

type Parser<'a> = char list -> ('a * char list) option
let map f (p:Parser<'a>): Parser<'b> = 
    p >> Option.map (fun (x, rest) -> f x, rest)

let cons x y = x :: y

let rec parseGroup: Group Parser= 
    let rec innerParse = 
        function 
        | '}'::rest -> Some ([], rest)
        | ',':: text
        | text ->
            match parseGroupOrGarbate text with 
            | Some (Choice1Of2 group,rest) -> map (cons group) innerParse rest
            | Some (Choice2Of2 _,rest) -> innerParse rest
            | None -> None

    function
    | '{' :: rest -> map Group innerParse rest
    | _ -> None 


and parseGarbage: Garbage Parser=
    let rec innerParse = 
        function 
        | '!'::_::rest -> innerParse rest
        | '>'::rest -> Some ((), rest)
        | _ -> None

    function
    | '<' :: rest -> (innerParse rest)
    | _ -> None 


and parseGroupOrGarbate: Choice<Group, Garbage> Parser= 
    function
    | '{'::rest -> map Choice1Of2 parseGroup <| '{'::rest
    | '<'::rest -> map Choice2Of2 parseGarbage <| '<'::rest
    | _ -> None


let path file = System.IO.Path.Combine (__SOURCE_DIRECTORY__, file)

let actualInput = System.IO.File.ReadAllText <| path "./9.txt"

let testInput = [|
    "{}" // score of 1.
    "{{{}}}" // score of 1 + 2 + 3 = 6.
    "{{},{}}" // score of 1 + 2 + 2 = 5.
    "{{{},{},{{}}}}" // score of 1 + 2 + 3 + 3 + 3 + 4 = 16.
    "{<a>,<a>,<a>,<a>}" // score of 1.
    "{{<ab>},{<ab>},{<ab>},{<ab>}}" // score of 1 + 2 + 2 + 2 + 2 = 9.
    "{{<!!>},{<!!>},{<!!>},{<!!>}}" // score of 1 + 2 + 2 + 2 + 2 = 9.
    "{{<a!>},{<a!>},{<a!>},{<ab>}}"// score of 1 + 2 = 3.
|]
let testGarbage = [|
    "<>"
    "<random characters>"
    "<<<<>"
    "<{!>}>"
    "<!!>"
    "<!!!>>"
    """<{o"i!a,<{i<a>"""
|]

