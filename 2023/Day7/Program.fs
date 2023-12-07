open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

module Day7 =
    
    let ReadData (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    
    type CardType=
        | Joker = 1u
        | Two = 2u
        | Three = 3u
        | Four = 4u
        | Five= 5u
        | Six=6u
        | Seven = 7u
        | Eight= 8u
        | Nine=9u
        | T = 10u
        | J = 11u
        | Q = 12u
        | K = 13u
        | A = 14u
    
    type HandType=
        | HighCard = 0u
        | OnePair = 1u
        | TwoPair = 2u
        | ThreeOfAKind = 3u
        | FullHouse = 4u
        | FourOfAKind = 5u
        | FiveOfAKind = 6u
    
    type Hand =
        struct
            val Bid : int
            val Cards : array<CardType>
            new (bid:int, hand : string ) = {Bid = bid; Cards = hand|> Seq.map(fun x -> match x with
                                                                                            | '2' -> CardType.Two
                                                                                            | '3' -> CardType.Three
                                                                                            | '4' -> CardType.Four
                                                                                            | '5' -> CardType.Five
                                                                                            | '6' -> CardType.Six
                                                                                            | '7' -> CardType.Seven
                                                                                            | '8' -> CardType.Eight
                                                                                            | '9' -> CardType.Nine
                                                                                            | 'T' -> CardType.T
                                                                                            | 'J' -> CardType.J
                                                                                            | 'Q' -> CardType.Q
                                                                                            | 'K' -> CardType.K
                                                                                            | 'A' -> CardType.A
                                                                                            | _ -> failwith "unknown card character")
            
                                                                    |> Seq.toArray}
            
            member this.FourOfAKindOrFullHouse(distinctCards:array<CardType>) : Option<HandType> =
                let searchedCard = distinctCards |> Array.head
                let numberOfFirstElement = (0,this.Cards)
                                           ||> Seq.fold(fun acc x ->
                                                            if x = searchedCard then
                                                                acc + 1
                                                            else
                                                                acc)
                match numberOfFirstElement with
                | 1 -> Some(HandType.FourOfAKind)
                | 2 -> Some(HandType.FullHouse)
                | 3 -> Some(HandType.FullHouse)
                | 4 -> Some(HandType.FourOfAKind)
                | _ -> None
                
            member this.ThreeOfAKindOrTwoPairs(distinctCards:array<CardType>) : Option<HandType> =
                let firstSearchedCard = distinctCards |> Array.head
                let secondSearchedCard = distinctCards |> Array.skip(1) |> Array.head
                // to refactor some day
                let numberOfFirstElement = (0,this.Cards)
                                           ||> Seq.fold(fun acc x ->
                                                            if x = firstSearchedCard then
                                                                acc + 1
                                                            else
                                                                acc)
                let numberOfSecondElement = (0,this.Cards)
                                           ||> Seq.fold(fun acc x ->
                                                            if x = secondSearchedCard then
                                                                acc + 1
                                                            else
                                                                acc)
                match numberOfFirstElement with
                // when there are two elements that are different out of three we're sure it's a three of a kind
                | 1 when numberOfSecondElement = 1 -> Some(HandType.ThreeOfAKind)
                | 1 when numberOfSecondElement = 2 -> Some(HandType.TwoPair)
                | 2 when numberOfSecondElement = 2 -> Some(HandType.TwoPair)
                | 2 when numberOfSecondElement = 1 -> Some(HandType.TwoPair)
                | 3 -> Some(HandType.ThreeOfAKind)
                | _ when numberOfSecondElement = 3 -> Some(HandType.ThreeOfAKind)
                | _ -> None
            
            member this.GetHandType  : HandType =
                let arrayOfDistinctCards = this.Cards |> Array.distinct
                let numberOfSameCards =  arrayOfDistinctCards |> Array.length
                match numberOfSameCards with
                | 1 -> HandType.FiveOfAKind
                | 2 -> match this.FourOfAKindOrFullHouse arrayOfDistinctCards with
                        | Some(kind) -> kind
                        | None -> failwith "not a full house or four of a kind"
                | 3 -> match this.ThreeOfAKindOrTwoPairs arrayOfDistinctCards with
                        | Some(kind) -> kind
                        | None -> failwith "not a three of a kind or two pair"
                | 4 -> HandType.OnePair
                | _ -> HandType.HighCard
                
            member this.GetAbsoluteRank : uint64 =
                // idea, use bitwise operation to generate the absolute ranking of the hand
                // there are five types, and then each card goes from 2-14, so it should be mappable to a
                // 6 * 4bit -> 24 bit number
                // X A B C D E
                // X -> the Hand type from 0->1
                // A -> The first card
                // B -> The second card
                // C -> The third card
                // D -> the fourth card
                // E -> the fifth card
                // since the hand type is the main value, it's first, and then the tie breaker happens from the first card and so on
                let handType = this.GetHandType
                let handValue = (0uL,this.Cards |> Array.indexed) ||> Array.fold(fun acc x ->
                                                                                let (index, cardType) = x
                                                                                let value = uint64(cardType) <<< 4*(4-index)
                                                                                value + acc   )
                handValue + (uint64(handType) <<< 4*5)
                
        end
    
    
    type HandWithJoker =
        struct
            val Bid : int
            val Cards : array<CardType>
            new (bid:int, hand : string ) = {Bid = bid; Cards = hand|> Seq.map(fun x -> match x with
                                                                                            | '2' -> CardType.Two
                                                                                            | '3' -> CardType.Three
                                                                                            | '4' -> CardType.Four
                                                                                            | '5' -> CardType.Five
                                                                                            | '6' -> CardType.Six
                                                                                            | '7' -> CardType.Seven
                                                                                            | '8' -> CardType.Eight
                                                                                            | '9' -> CardType.Nine
                                                                                            | 'T' -> CardType.T
                                                                                            | 'J' -> CardType.Joker
                                                                                            | 'Q' -> CardType.Q
                                                                                            | 'K' -> CardType.K
                                                                                            | 'A' -> CardType.A
                                                                                            | _ -> failwith "unknown card character")
            
                                                                    |> Seq.toArray}
            
            member this.GetNumberOfThisCard (searchedCard:CardType) : int =
                (0,this.Cards) ||> Seq.fold(fun acc x ->
                                                    if x = searchedCard then
                                                        acc + 1
                                                    else
                                                        acc)
            
            member this.FourOfAKindOrFullHouse(distinctCards:array<CardType>) : Option<HandType> =
                let searchedCard = distinctCards |> Array.find(fun x -> not (x = CardType.Joker))
                let numberOfFirstElement = this.GetNumberOfThisCard searchedCard
                let containsJoker = distinctCards |> Array.contains(CardType.Joker)
                match numberOfFirstElement with
                | 1 -> Some(HandType.FourOfAKind)
                | 2  -> Some(HandType.FullHouse)
                | 3 when not containsJoker -> Some(HandType.FullHouse)
                | 4 -> Some(HandType.FourOfAKind)
                | _ -> Some(HandType.FourOfAKind) // joker kind return the best
            
            member this.ThreeOfAKindOrTwoPairs(distinctCards:array<CardType>) : Option<HandType> =
                let firstSearchedCard = distinctCards |> Array.find(fun x -> not (x = CardType.Joker))
                let secondSearchedCard = distinctCards |> Array.skip(1) |> Array.find(fun x -> not (x = CardType.Joker))
                // to refactor some day
                let numberOfFirstElement = this.GetNumberOfThisCard firstSearchedCard
                let numberOfSecondElement = this.GetNumberOfThisCard secondSearchedCard
                let containsJoker = distinctCards |> Array.contains(CardType.Joker)
                match numberOfFirstElement with
                // when there are two elements that are different out of three we're sure it's a three of a kind
                | 1 when numberOfSecondElement = 1 -> Some(HandType.ThreeOfAKind)
                | 1 when numberOfSecondElement = 2 && not containsJoker -> Some(HandType.TwoPair)
                | 2 when numberOfSecondElement = 2 && not containsJoker -> Some(HandType.TwoPair)
                | 2 when numberOfSecondElement = 1 && not containsJoker -> Some(HandType.TwoPair)
                | 3 -> Some(HandType.ThreeOfAKind)
                | _ when numberOfSecondElement = 3 -> Some(HandType.ThreeOfAKind)
                | _ -> Some(HandType.ThreeOfAKind) // joker case, return the best
            
            member this.GetHandType  : HandType =
                let arrayOfDistinctCards = this.Cards |> Array.distinct
                let numberOfSameCards =  arrayOfDistinctCards |> Array.length
                let numberOfJokers = match arrayOfDistinctCards |> Array.contains(CardType.Joker) with
                                        | true -> this.GetNumberOfThisCard CardType.Joker
                                        | false -> 0
                match numberOfSameCards with
                | 1 -> HandType.FiveOfAKind
                | 2 when numberOfJokers >=1 -> HandType.FiveOfAKind
                | 2 -> match this.FourOfAKindOrFullHouse arrayOfDistinctCards with
                        | Some(kind) -> kind
                        | None -> failwith "not a full house or four of a kind"
                | 3 when numberOfJokers = 1 -> match this.FourOfAKindOrFullHouse arrayOfDistinctCards with
                                                                                    | Some(kind) -> kind
                                                                                    | None -> failwith "not a three of a kind or two pair"
                | 3 when numberOfJokers = 2 -> HandType.FourOfAKind
                | 3 when numberOfJokers = 3 -> HandType.FourOfAKind
                | 3 -> match this.ThreeOfAKindOrTwoPairs arrayOfDistinctCards with
                        | Some(kind) -> kind
                        | None -> failwith "not a three of a kind or two pair"
                | 4 when numberOfJokers = 1  -> match this.ThreeOfAKindOrTwoPairs arrayOfDistinctCards with
                                                                                    | Some(kind) -> kind
                                                                                    | None -> failwith "not a three of a kind or two pair"
                | 4 when numberOfJokers = 2  -> HandType.ThreeOfAKind   
                | 4 -> HandType.OnePair
                | 5 when numberOfJokers = 1 -> HandType.OnePair
                | _ -> HandType.HighCard
                
            member this.GetAbsoluteRank : uint64 =
                // idea, use bitwise operation to generate the absolute ranking of the hand
                // there are five types, and then each card goes from 2-14, so it should be mappable to a
                // 6 * 4bit -> 24 bit number
                // X A B C D E
                // X -> the Hand type from 0->1
                // A -> The first card
                // B -> The second card
                // C -> The third card
                // D -> the fourth card
                // E -> the fifth card
                // since the hand type is the main value, it's first, and then the tie breaker happens from the first card and so on
                let handType = this.GetHandType
                let handValue = (0uL,this.Cards |> Array.indexed) ||> Array.fold(fun acc x ->
                                                                                let (index, cardType) = x
                                                                                let value = uint64(cardType) <<< 4*(4-index)
                                                                                value + acc   )
                handValue + (uint64(handType) <<< 4*5)
                
        end
    
    let RunStarOne (filePath:string) : int =
        let test = ReadData filePath
        ReadData filePath |> Seq.map(fun x ->
                                            let splitLine = x.Split(' ')
                                            match splitLine with
                                            |  [|hand;bid|] -> Hand(bid |> int, hand)
                                            |_ -> failwith "unknown format")
                          |> Seq.sortBy(fun x -> x.GetAbsoluteRank)
                          |> Seq.mapi(fun i x -> (i+1)*x.Bid)
                          |> Seq.sum
    
    let RunStarTwo (filePath:string) : int =
        let test = ReadData filePath
        ReadData filePath |> Seq.map(fun x ->
                                            let splitLine = x.Split(' ')
                                            match splitLine with
                                            |  [|hand;bid|] -> HandWithJoker(bid |> int, hand)
                                            |_ -> failwith "unknown format")
                          |> Seq.sortBy(fun x -> x.GetAbsoluteRank)
                          |> Seq.mapi(fun i x -> (i+1)*x.Bid)
                          |> Seq.sum
        
    

Day7.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day7.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"
    