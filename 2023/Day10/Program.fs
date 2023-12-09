open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

module Day10 =
    
    let ReadData (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    
    let RunStarOne (filePath:string) : int = 0
    
    let RunStarTwo (filePath:string) : int = 0 
        
    

Day10.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day10.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"