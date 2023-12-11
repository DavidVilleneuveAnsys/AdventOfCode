open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

module Day13 =
    
    let ReadData (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    
    let RunStarOne (filePath:string) : int = 0
    
    let RunStarTwo (filePath:string) : int = 0 
        
    

Day13.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day13.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"