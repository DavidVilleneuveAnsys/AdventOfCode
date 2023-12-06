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
    let GetDistancePerPressingTime (pressingTime:int) (totalTime:int) : int =
        let speed = pressingTime
        let remainingTime = totalTime - pressingTime
        speed*remainingTime
    
    let IsRaceResultValid(raceResult:int*int) (distanceToBeat:int) : bool =
        match raceResult with
        | (pressingTime, givenDistance) when givenDistance > distanceToBeat -> true
        | _ -> false
    
    let NumberOfWinningCases(time:int) (distance:int) : int =
        let possibleDistances =  seq {
            for i in 0..time do
                yield (i, GetDistancePerPressingTime i time)
        }
        let (lowerBound,_) = possibleDistances |> Seq.find(fun x -> IsRaceResultValid x distance)
        let (upperBound,_) = possibleDistances |> Seq.findBack(fun x -> IsRaceResultValid x distance)
        (upperBound - lowerBound)+1
        
    
    let RunStarOne (filePath:string) : int =
        let rx = Regex(@"\d+", RegexOptions.Compiled)
        let data = ReadData filePath
        let times = data |> Seq.head |> rx.Matches |> Seq.map(fun x -> x.Value |> int)
        let distances = data |> Seq.last |> rx.Matches |> Seq.map(fun x -> x.Value |> int)
        let timesAndDistances = (times, distances) ||> Seq.zip 
        timesAndDistances |> Seq.map(fun x -> match x with
                                                | (time, distance) -> NumberOfWinningCases time distance)
                          |> Seq.reduce(fun x y -> x*y)
    
    let RunStarTwo (filePath:string) : int = 0 
        
    

Day6.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day6.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"
    