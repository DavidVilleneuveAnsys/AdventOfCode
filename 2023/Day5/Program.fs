open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions


module Day5 =
    let ReadData (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    
    
    type Overlap =
       | NoOverlap = 0
       | Contained = 1
       | OverlapStart = 1
       | OverlapEnd = 3
       
    
    type Mapping =
        struct
            val Source : int64
            val Destination : int64
            val Range : int64
            new (source:int64, destination: int64, range:int64) = {Source = source; Destination = destination; Range = range}
            
            member this.IsInRange (value : int64) : bool =
                value >= this.Source && value < (this.Source + this.Range)
            
            member this.MapValue(value : int64) : int64 =
                if not (this.IsInRange value) then
                    failwith "value must be in range"
                (value - this.Source) + this.Destination
            
            member this.RangeOverlapsMapping(start:int64, max:int64 ) :  Overlap =
                    let endInRange = this.IsInRange max
                    let startInRange = this.IsInRange start
                    match startInRange with
                    | true when endInRange = true -> Overlap.Contained
                    | true when endInRange = false -> Overlap.OverlapEnd
                    | false when endInRange = true -> Overlap.OverlapStart
                    | _ -> Overlap.NoOverlap
        end
    
    let ReadMapping(mappingLines:array<string>) : seq<Mapping> =
        mappingLines |> Seq.map (fun x ->
                                    match x.Split(" ") with
                                    | [|destination;source;step|] -> Mapping(source|> int64, destination |>int64, step |> int64)
                                    | _ -> failwith "line doesn't have the right format" )
        
    let CreateMappings (lines:seq<string>) =
        0
    
    let ConvertNumberUsingMappings (mappings:seq<Mapping> ) (value:int64) :int64 = 
        match (mappings |> Seq.tryFind(fun x -> x.IsInRange value)) with
        | None -> value
        | Some mapping -> mapping.MapValue value
    
    let CreateMappingsFromLines(lines:seq<string>) : seq<seq<Mapping>> =
        let GetAllNumbersRegex = Regex(@"\d+", RegexOptions.Compiled)
        seq{
            let lineArray = lines |> Seq.skip(1) |> Seq.toArray
            let mutable position = 0
            while position >= 0 do
                let startOfArray = Array.FindIndex(lineArray, position, fun x -> GetAllNumbersRegex.IsMatch(x))
                let endOfArray = Array.FindIndex(lineArray, startOfArray, fun x -> not(GetAllNumbersRegex.IsMatch(x)))
                let mappingArray = match endOfArray with
                                    | -1 -> Array.sub lineArray startOfArray (lineArray.Length-startOfArray)
                                    | _  -> Array.sub lineArray startOfArray (endOfArray-startOfArray)
                yield ReadMapping mappingArray
                position <- endOfArray
        }
    
    let ApplyMappingsOnSeeds(seeds:seq<int64>) (mappings:seq<seq<Mapping>>): seq<int64> =
        seeds |>  Seq.map(fun x ->
                            (x ,mappings) ||> Seq.fold(fun acc m -> ConvertNumberUsingMappings m acc ) )
        
    let ApplyMappingOnSeedsParallel(seeds:seq<int64>) (mappings:seq<seq<Mapping>>) : seq<int64> =
        seeds |> Seq.toArray |> Array.Parallel.map(fun x ->
                                (x ,mappings) ||> Seq.fold(fun acc m -> ConvertNumberUsingMappings m acc ) ) |> Array.toSeq
    
    let MapSeedsToLocation(lines:seq<string>) : seq<int64> =
        let GetAllNumbersRegex = Regex(@"\d+", RegexOptions.Compiled)
        // Get the first line, it represents the seeds
        let seeds = lines |> Seq.take(1) |> Seq.head |> GetAllNumbersRegex.Matches |> Seq.map(fun x -> x.Value |> int64)
        // parse the other line to generate multiple sequence from mappings
        let mappings = CreateMappingsFromLines lines
        ApplyMappingsOnSeeds seeds mappings
        
    let MapAllSeedsToLocation(lines:seq<string>) : seq<int64> =
        let GetAllNumbersRegex = Regex(@"\d+", RegexOptions.Compiled)
        // Get the first line, it represents the seeds
        let seeds = lines |> Seq.take(1) |> Seq.head |> GetAllNumbersRegex.Matches |> Seq.map(fun x -> x.Value |> int64)
        let seedsAndRange = seeds |> Seq.indexed |> Seq.toArray |> Array.partition(fun x ->
                                                                let (i,_) = x
                                                                i % 2 = 0 ) 
        let allSeeds = seq{
            for i in seedsAndRange ||> Array.zip do
                let (_,seed),(_,range) = i
                for j in seed..(seed+range) do
                    yield j
        }
        // parse the other line to generate multiple sequence from mappings
        let mappings = CreateMappingsFromLines lines
        ApplyMappingOnSeedsParallel allSeeds mappings
        
    let MapAllSeedsToLocation2(lines:seq<string>)   = 
        let GetAllNumbersRegex = Regex(@"\d+", RegexOptions.Compiled)
        // Get the first line, it represents the seeds
        let seeds = lines |> Seq.take(1) |> Seq.head |> GetAllNumbersRegex.Matches |> Seq.map(fun x -> x.Value |> int64)
        let seedsAndRange = seeds |> Seq.indexed |> Seq.toArray |> Array.partition(fun x ->
                                                                let (i,_) = x
                                                                i % 2 = 0 )
        let seedRange = seq{
            for i in seedsAndRange ||> Array.zip do
                let (_,seed),(_,range) = i
                yield seed
                yield seed+range
        }
        let seedRangeArray = seedRange |> Seq.toArray
        let mappings = CreateMappingsFromLines lines
        let rangeMapping = ApplyMappingsOnSeeds seedRangeArray mappings |> Seq.toArray
        for i in 0 .. 2 .. rangeMapping.Length do
            if rangeMapping[i+1] - rangeMapping[0] > seedRangeArray[i+1] then
                failwith "found mapping is bigger that original mapping"
        
    let RunStarOne (filePath:string) : int64 =
        ReadData filePath |> MapSeedsToLocation |> Seq.min 
    let RunStarTwo (filePath:string) : int64 =
        ReadData filePath |> MapAllSeedsToLocation |> Seq.min 
        
    

Day5.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day5.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"
    