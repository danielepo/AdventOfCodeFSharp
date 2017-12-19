//#I "."
//__SOURCE_DIRECTORY__
//__SOURCE_FILE__
//__LINE__
module Day9
open System
type Parser<'a> = char list -> ('a*char list) option

let map f p =
  p >> Option.map (fun (x, rest) -> (f x), rest)

type Group = Group of Group list

type Garbage = unit

let cons x xs = x::xs

let rec parseGroup: Group Parser =
  let rec parseInner = function
    | '}'::rest -> Some ([], rest)
    | ','::text
    | text ->
        match parseGroupOrGarbage text with
          | Some (Choice1Of2 group, rest) -> map (cons group) parseInner rest
          | Some (Choice2Of2 _, rest)-> parseInner rest
          | None -> None

  function
  | '{'::inner -> map Group parseInner inner
  | _ -> None

and parseGarbage: Garbage Parser =
  let rec parseInner = function
    | '!'::_::rest -> parseInner rest
    | '>'::rest -> Some ((), rest)
    | _::rest -> parseInner rest

  function
  | '<'::inner -> parseInner inner
  | _ -> None

and parseGroupOrGarbage: Choice<Group, Garbage> Parser = function
  | '{'::inner -> map Choice1Of2 parseGroup ('{'::inner)
  | '<'::inner -> map Choice2Of2 parseGarbage ('<'::inner)
  | _ -> None

let score group =
  let rec scoreByLevel n (Group children) =
    n + Seq.sumBy (scoreByLevel (n + 1)) children


  scoreByLevel 1 group
let path file = System.IO.Path.Combine (__SOURCE_DIRECTORY__, file)

let actualInput = System.IO.File.ReadAllText <| path "./9.txt"



actualInput
  |> List.ofSeq
  |> parseGroup
  |> Option.iter (fst >> score >> printfn "%d")




let rec clean trash input:char list = 
    match input, trash with
    | '!'::_:: xs,true -> clean true xs
    | '>' :: xs, true -> clean false xs 
    | '<' :: xs,false -> clean true xs 
    | _ :: xs,true -> clean true     xs 
    | c :: xs,false ->c :: clean false    xs
    | [], _ -> []

let rec count group input =
    match input with
    | '{'::xs -> group + count (group + 1) xs
    | '}' :: xs-> count (group - 1) xs
    | _ :: xs-> count (group) xs
    | []-> 0

actualInput
    |> List.ofSeq
    |> clean false
    |> count 1
    |> ignore