module Utils
open System.IO

let TextFileReader (path:string) = seq {
        use stream = new  StreamReader(path)
        while not stream.EndOfStream do
            yield stream.ReadLine()
        } 
