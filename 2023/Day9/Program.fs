open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

module Day9 =
    
    let ReadData (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    
    let GetNumbersFromLine (line:string) : array<int64> =
        let rx = Regex(@"\d+", RegexOptions.Compiled)
        line |> rx.Matches |> Seq.map(fun x -> x.Value |> int64) |> Seq.toArray
        
        
    let GetDiffArray (line:array<int64>) : array<int64>  =
        let emptyArray = Array.empty<int64>
        (emptyArray, line |> Array.pairwise) ||> Array.fold(fun acc x -> let (first, second) = x
                                                                         let diff = second-first
                                                                         [|diff|] |> Array.append acc ) 
    
    let GetLineNextNumber(line:string) : int64 =
        let initialLineValues = GetNumbersFromLine line
        let intermediateValues = initialLineValues |> Array.unfold(fun x ->
                                                                    if x |> Array.forall(fun x -> x = 0) then
                                                                        None
                                                                    else
                                                                        let nextArray = GetDiffArray x
                                                                        Some(x,nextArray)) |> Array.rev
        let sum = (0L,intermediateValues) ||> Array.fold(fun acc x ->
                                                        let lastItem = x |> Array.last
                                                        acc + lastItem )
        sum
    
    
    let RunStarOne (filePath:string) : int64 =
        ReadData filePath |> Seq.map GetLineNextNumber |> Seq.sum
    
    let RunStarTwo (filePath:string) : int = 0 
        
    

Day9.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day9.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"