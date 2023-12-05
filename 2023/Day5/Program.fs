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
            
            member this.IsInRange (value : int) : bool =
                value >= this.Source && value < (this.Source + this.Range)
            
            member this.MapValue(value : int) : int =
                if not (this.IsInRange value) then
                    failwith "value must be in range"
                (value - this.Source) + this.Destination
        end
    
    let ReadMapping(mappingLines:seq<string>) : seq<Mapping> =
        mappingLines |> Seq.map (fun x ->
                                    match x.Split(" ") with
                                    | [|destination;source;step|] -> Mapping(source|> int, destination |>int, step |> int)
                                    | _ -> failwith "line doesn't have the right format" )
        
    let CreateMappings (lines:seq<string>) =
        0
        
    let ConvertNumberUsingMappings (mappings:seq<Mapping> ) (value:int) :int = 
        match (mappings |> Seq.tryFind(fun x -> x.IsInRange value)) with
        | None -> value
        | Some mapping -> mapping.MapValue value
        
        
    let RunStarOne (filePath:string) : int =
        ReadData filePath |> CreateMappings 
    let RunStarTwo (filePath:string) : int = 0
        
    

Day5.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day5.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"
    