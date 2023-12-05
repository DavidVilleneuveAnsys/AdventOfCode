open System
open System.IO
open System.Collections.Generic

module Day5 =
    
    
    let ReadData (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    
    type Mapping =
        struct
            val Source : int
            val Destination : int
            val Range : int
            new (source:int, destination: int, range:int) = {Source = source; Destination = destination; Range = range}
            
            member this.IsInRange (value : int) : bool = true
            
            member this.MapValue(value : int) int =
                if not (this.IsInRange value) then
                    failwith "value must be in range"
                0
        end
    
    
    let RunStarOne (filePath:string) : int = 0
    let RunStarTwo (filePath:string) : int = 0
        
    

Day5.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day5.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"
    