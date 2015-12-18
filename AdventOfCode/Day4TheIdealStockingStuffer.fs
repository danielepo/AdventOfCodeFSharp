module Day4TheIdealStockingStuffer
let preString = [for a in [1..6] do yield "0" ] |> List.reduce (+)

let IsGem input =
    let MD5Hash (input : string) =
        use md5 = System.Security.Cryptography.MD5.Create()
        input
        |> System.Text.Encoding.ASCII.GetBytes
        |> md5.ComputeHash
        |> Seq.map (fun c -> c.ToString("X2"))
        |> Seq.reduce (+)
    (MD5Hash input).StartsWith(preString)

let rec Mine input (dec:int64) =
    let rock = sprintf "%s%d" input dec
    rock
    |> IsGem
    |> function
        | true -> dec
        | false -> Mine input (dec + 1L)