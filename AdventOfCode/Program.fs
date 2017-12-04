// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open Stars

[<EntryPoint>]
let main argv = 
    
    let day1input1 = System.IO.File.ReadAllText("./day1input1.txt")
    

    printf "%d" <| calculateCapcha day1input1
    0 // return an integer exit code
