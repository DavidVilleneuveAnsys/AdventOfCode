open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

module Day6 =
    
    let ReadData (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    
    // well that is technically dumb
    let GetDistancePerPressingTime (pressingTime:int64) (totalTime:int64) : int64 =
        let speed = pressingTime
        let remainingTime = totalTime - pressingTime
        speed*remainingTime
    
    let IsRaceResultValid(raceResult:int64*int64) (distanceToBeat:int64) : bool =
        match raceResult with
        | (pressingTime, givenDistance) when givenDistance > distanceToBeat -> true
        | _ -> false
    
    let NumberOfWinningCases(time:int64) (distance:int64) : int64 =
        let possibleDistances =  seq {
            for i in int64(0)..time do
                yield (i, GetDistancePerPressingTime i time)
        }
        let (lowerBound,_) = possibleDistances |> Seq.find(fun x -> IsRaceResultValid x distance)
        let (upperBound,_) = possibleDistances |> Seq.findBack(fun x -> IsRaceResultValid x distance)
        (upperBound - lowerBound)+int64(1)
        
    
    let RunStarOne (filePath:string) : int64 =
        let rx = Regex(@"\d+", RegexOptions.Compiled)
        let data = ReadData filePath
        let times = data |> Seq.head |> rx.Matches |> Seq.map(fun x -> x.Value |> int64)
        let distances = data |> Seq.last |> rx.Matches |> Seq.map(fun x -> x.Value |> int64)
        let timesAndDistances = (times, distances) ||> Seq.zip 
        timesAndDistances |> Seq.map(fun x -> match x with
                                                | (time, distance) -> NumberOfWinningCases time distance)
                          |> Seq.reduce(fun x y -> x*y)
    
    let RunStarTwo (filePath:string) : int64 =
        let rx = Regex(@"\d+", RegexOptions.Compiled)
        let data = ReadData filePath
        let time = data |> Seq.head |> String.filter(fun x -> x |> Char.IsNumber) |> int64
        let distance = data |> Seq.last |> String.filter(fun x -> x |> Char.IsNumber) |> int64
        NumberOfWinningCases time distance
        
    

Day6.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day6.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"
    