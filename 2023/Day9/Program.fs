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
    
    let GetNumbersFromLine (line:string) : array<int> =
        let rx = Regex(@"\d+", RegexOptions.Compiled)
        line |> rx.Matches |> Seq.map(fun x -> x.Value |> int) |> Seq.toArray
        
        
    let GetDiffArray (line:array<int>) : array<int>  =
        let emptyArray = Array.empty<int>
        (emptyArray, line |> Array.pairwise) ||> Array.fold(fun acc x -> let (first, second) = x
                                                                         let diff = second-first
                                                                         acc |> Array.append [|diff|]) 
    
    let GetLineNextNumber(line:string) : int =
        let initialLineValues = GetNumbersFromLine line
        let intermediateValues = initialLineValues |> Array.unfold(fun x ->
                                                                    if x |> Array.forall(fun x -> x = 0) then
                                                                        None
                                                                    else
                                                                        let nextArray = GetDiffArray x
                                                                        Some(x,nextArray)) |> Array.rev
        let sum = (0,intermediateValues) ||> Array.fold(fun acc x ->
                                                        let lastItem = x |> Array.last
                                                        acc + lastItem )
        sum
    
    
    let RunStarOne (filePath:string) : int =
        ReadData filePath |> Seq.map GetLineNextNumber |> Seq.sum
    
    let RunStarTwo (filePath:string) : int = 0 
        
    

Day9.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day9.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"