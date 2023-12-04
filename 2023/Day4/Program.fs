open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

module Day4 =
    
    
    type Card =
        struct
            val Number : int
            val Value : int
            new (n:int, value : int) = {Number = n; Value = value;}
        end
    
    let ReadData (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    
    let GetCardFromString(card:string) : Card =
        let rx = Regex(@"\d+", RegexOptions.Compiled)
        let splitCard = card.Split(":")
        let data = splitCard[1]
        let cardNumberStr = splitCard[0] |> rx.Matches |> Seq.head
        let cardNumber= cardNumberStr.Value |> int
        let cardNumbers = data.Split("|")
        let actual = cardNumbers[0] |> rx.Matches |> Seq.map(fun x -> x.Value |> int)
        let expected = cardNumbers[1] |> rx.Matches |> Seq.map(fun x -> x.Value |> int)
        let nbCard = actual |> Seq.where(fun x -> expected |> Seq.contains x) |> Seq.length
        Card(cardNumber,nbCard)
    
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
    
    
    let GetNumberOfCards(listOfCards:seq<string>) : seq<int> =
        // note I am not sure that sorting is really important in this case, but it seems like when using sequence they weren't treated in order .. better same than sorry
        let cards = listOfCards |> Seq.map GetCardFromString
        let sortedCards = cards |> Seq.sortBy(fun x -> x.Number)
        let initialNumberOfCards = sortedCards |> Seq.length
        let initialValues:int array = [| for _ in 0 .. initialNumberOfCards-1 -> 1 |]
        let multipliers = (initialValues, sortedCards) ||> Seq.fold(fun acc x ->
                                                    acc |> Array.mapi(fun i n ->
                                                                        if i > (x.Number - 1) && i < (x.Number + x.Value ) then
                                                                            n + acc[x.Number-1]
                                                                        else
                                                                            n
                                                                        )) |> Array.toSeq
        multipliers
    
    
    let RunStarTwo (filePath:string) : int =
        ReadData filePath |> GetNumberOfCards |> Seq.sum
        
    

Day4.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day4.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"
    