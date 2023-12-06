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
       | OverlapStart = 2
       | OverlapEnd = 3
       | Encompasses = 4
       
    
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
                    | false when endInRange = false && start < this.Source && max > (this.Source+this.Range-int64(1)) -> Overlap.Encompasses
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
    
    
    // bad brute-force implementation that takes for ever    
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
    
    let TransformRangeWithMapping (min:int64,max:int64) (mapping:Mapping) : seq<int64 * int64> =
        match mapping.RangeOverlapsMapping (min,max) with
        | Overlap.NoOverlap -> seq{(min,max)}
        | Overlap.Contained -> seq{(mapping.MapValue min, mapping.MapValue max)}
        | Overlap.OverlapEnd ->
            let mappedMin = mapping.MapValue min
            let maxMappedValue = mapping.MapValue (mapping.Source + mapping.Range - int64(1))
            let mappedRangeSize = maxMappedValue - mappedMin
            seq{
                (mappedMin, maxMappedValue)
                (min+mappedRangeSize+int64(1), max)
            }            
        | Overlap.OverlapStart ->
            let mappedMax = mapping.MapValue max
            let minMappedRange = mapping.Source - int64(1)
            let mappedRangeStart = mapping.MapValue mapping.Source
            seq{
                (min, minMappedRange)
                (mappedRangeStart, mappedMax)
            }
        | Overlap.Encompasses ->
            let minMappedRange = mapping.Source - int64(1)
            let mappedRangeStart = mapping.MapValue mapping.Source
            let highestSourceValue = mapping.Source + mapping.Range - int64(1)
            let mappedRangeEnd = mapping.MapValue highestSourceValue
            seq{
                (min, minMappedRange)
                (mappedRangeStart, mappedRangeEnd)
                (highestSourceValue+int64(1), max)
            }
    
    
    let TransformRangeWithMappings (min:int64,max:int64) (mappings:seq<seq<Mapping>>) : seq<int64 * int64> =
        let firstElement = seq{ (min,max) }
        (firstElement, mappings) ||> Seq.fold(fun acc currentMapping ->
                                                    let ranges = seq{
                                                        for r in acc do
                                                            let newRanges = currentMapping  |> Seq.map( fun m -> TransformRangeWithMapping r m)
                                                                                            |> Seq.concat
                                                            let (rn,rx) = r
                                                            let rangeSize = rx-rn
                                                            let mutable numberOfValuesSent = int64(0)
                                                            let mutable unchangedMaps = List.empty
                                                            for nr in newRanges do
                                                                let (min , max) = nr
                                                                let currentRangeSize = max-min
                                                                if min >= rn && max <= rx then
                                                                    unchangedMaps <- unchangedMaps |>List.append [nr]
                                                                else
                                                                    numberOfValuesSent <- (numberOfValuesSent + currentRangeSize)
                                                                    yield nr
                                                            if numberOfValuesSent < rangeSize then
                                                                let amountOfMissingItems = rangeSize - numberOfValuesSent
                                                                let unMappedRange = unchangedMaps |> List.tryPick(fun x ->
                                                                                                        let (s,e) = x
                                                                                                        if amountOfMissingItems = e-s then
                                                                                                            Some(x)
                                                                                                        else
                                                                                                             None
                                                                                                        )
                                                                match unMappedRange with
                                                                | Some(unMappedRange) -> yield unMappedRange
                                                                | None -> ()
                                                         
                                                    } 
                                                    ranges |> Seq.sortBy(fun x ->
                                                                                let (min,_) = x
                                                                                min)
                                                    )
    
    let GetAllRanges (ranges:seq<int64*int64>) (mappings:seq<seq<Mapping>>) : seq<int64 * int64> =
        ranges |> Seq.map(fun x -> TransformRangeWithMappings x mappings) |> Seq.concat
     
    let MapAllSeedsToLocation2(lines:seq<string>) : seq<int64*int64>  = 
        let GetAllNumbersRegex = Regex(@"\d+", RegexOptions.Compiled)
        // Get the first line, it represents the seeds
        let seeds = lines |> Seq.take(1) |> Seq.head |> GetAllNumbersRegex.Matches |> Seq.map(fun x -> x.Value |> int64)
        let seedsAndRange = seeds |> Seq.indexed |> Seq.toArray |> Array.partition(fun x ->
                                                                let (i,_) = x
                                                                i % 2 = 0 )
        let mappings = CreateMappingsFromLines lines
        let seedRange = seq{
            for i in seedsAndRange ||> Array.zip do
                let (_,seed),(_,range) = i
                yield (seed, seed+range)
        }
        let sortedSeeds = seedRange |> Seq.sortBy(fun x ->
                                                    let (min,_) = x
                                                    min)
        
        GetAllRanges sortedSeeds mappings
        
        
    let RunStarOne (filePath:string) : int64 =
        ReadData filePath |> MapSeedsToLocation |> Seq.min 
    let RunStarTwo (filePath:string) : int64 =
        ReadData filePath   |> MapAllSeedsToLocation2
                            |> Seq.map(fun x ->
                                            let (min, _) = x
                                            min)
                            |> Seq.min
        
    

Day5.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day5.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"
    