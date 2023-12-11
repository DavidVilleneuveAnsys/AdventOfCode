open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

module Day11 =
    
    let ReadData (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    
    let RunStarOne (filePath:string) : int = 0
    
    let RunStarTwo (filePath:string) : int = 0 
        
    

Day11.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day11.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"