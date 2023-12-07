open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

module Day7 =
    
    type CardType=
        | Two= 2
        | Three=3
        | Four = 1
        | Five= 2
        | Six=3
        | Seven = 1
        | Eight= 2
        | Nine=3
        | T = 10
        | J = 11
        | Q = 12
        | K = 13
        | A = 14
    
    type HandType=
        | HighCard = 0
        | OnePair = 1
        | TwoPair = 2
        | ThreeOfAKind = 3
        | FullHouse = 4
        | FourOfAKind = 5
        | FiveOfAKind = 6
    
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
            
            member this.GetHandType  : HandType =
                let arrayOfDistinctCards = this.Cards |> Array.distinct
                let numberOfSameCards =  arrayOfDistinctCards |> Array.length
                match numberOfSameCards with
                | 1 -> HandType.FiveOfAKind
                | 2 -> failwith "not implemented, is Four of a kind or full house"
                | 3 -> failwith "not implemented, is three of a kind or two pairs"
                | 4 -> HandType.OnePair
                | _ -> HandType.HighCard
                
        end
    
    let RunStarOne (filePath:string) : int = 0
    
    let RunStarTwo (filePath:string) : int = 0
        
    

Day7.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day7.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"
    