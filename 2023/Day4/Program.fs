open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

module Day4 =
    
    
    let ReadData (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    
    let GetNumberOfMatchingNumbers(card:string) : int =
        let rx = Regex(@"\d+", RegexOptions.Compiled)
        let data = card.Split(":")[1]
        let cardNumbers = data.Split("|")
        let actual = cardNumbers[0] |> rx.Matches |> Seq.map(fun x -> x.Value |> int)
        let expected = cardNumbers[1] |> rx.Matches |> Seq.map(fun x -> x.Value |> int)
        actual |> Seq.where(fun x -> expected |> Seq.contains x) |> Seq.length
        
    let GetValueOfCard(card:string) : int =
        let nbOfNumbers = GetNumberOfMatchingNumbers card |> double
        if nbOfNumbers > 0 then
            2.0**(nbOfNumbers - 1.0) |> int
        else
            0
        
    let GetValueOfCards(listOfCards:seq<string>) : seq<int> =
        listOfCards |> Seq.map GetValueOfCard
    
    
    let RunStarOne (filePath:string) : int =
        ReadData filePath |> GetValueOfCards |> Seq.sum
    
    let GetNumberOfCard(card:string) : int =
        let nbOfNumbers = GetNumberOfMatchingNumbers card
        nbOfNumbers
    
    let GetNumberOfCards(listOfCards:seq<string>) : seq<int> =
        listOfCards |> Seq.map GetNumberOfCard
    
    let RunStarTwo (filePath:string) : int =
        ReadData filePath |> GetNumberOfCards |> Seq.sum
        
    

Day4.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day4.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"
    