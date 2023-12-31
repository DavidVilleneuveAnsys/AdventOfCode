module Tests

open System
open Program
open Xunit

//===========================Day 2 ===============================
[<Fact>]
[<Trait("Category","Day 2")>]
let ``Day 2 star 1 test Data`` () =
    let result = Day2.RunStarOne @".\ValidationData1.txt"
    Assert.Equal(8, result)

[<Fact>]
[<Trait("Category","Day 2")>]
let ``Day 2 star 2 test Data`` () =
    let result = Day2.RunStarTwo @".\ValidationData2.txt"
    Assert.Equal(2286, result)

[<Fact>]
[<Trait("Category","Day 2")>]
let ``Test architecture test`` () =
    Assert.All(Day2.ReadData @".\testData.txt"
               |> Seq.toList,
               fun x -> Assert.Equal(x,"test"))
    
[<Fact>]
[<Trait("Category","Day 2")>]
let ``test is possible`` () =
    let data = Day2.Game(0,1,2,3)
    Assert.True(data.IsPossible(1,2,3))
    Assert.False(data.IsPossible(1,1,1))
    Assert.False(data.IsPossible(0,0,0))
    Assert.False(data.IsPossible(0,0,4))
    Assert.False(data.IsPossible(0,3,0))
    Assert.False(data.IsPossible(2,2,2))
    Assert.True(data.IsPossible(2,3,4))


//======================================= Day 3 ===============================
[<Fact>]
[<Trait("Category","Day 3")>]
let ``Day 3 star 1 test Data`` () =
    let result = Day3.RunStarOne @".\ValidationData3-1.txt"
    Assert.Equal(4361, result)
    
[<Fact>]
[<Trait("Category","Day 3")>]
let ``Day 3 star 1 test Data2`` () =
    let result = Day3.RunStarOne @".\ValidationData3-1.2.txt"
    Assert.Equal(15634, result)

[<Fact>]
[<Trait("Category","Day 3")>]
let ``Day 3 star 1 test Data3`` () =
    let result = Day3.RunStarOne @".\ValidationData3-1.3.txt"
    Assert.Equal(6463, result)

[<Fact>]
[<Trait("Category","Day 3")>]
let ``Day 3 star 2 test Data`` () =
    let result = Day3.RunStarTwo @".\ValidationData3-2.txt"
    Assert.Equal(467835, result)
    
[<Fact>]
[<Trait("Category","Day 3")>]
let ``Get Symbols And Numbers From line`` () =
    let result = Day3.GetSymbolsAndNumbersFromLine ".........@....109$...*..........988........$..241..750.............@............169...162......*...%....575.743......*..................278." 5
    let expected = seq {
        yield Day3.SymbolChoice(Day3.Symbol(Day3.Position(9,5),'@'))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(14,5),3,109))
        yield Day3.SymbolChoice(Day3.Symbol(Day3.Position(17,5),'$'))
        yield Day3.SymbolChoice(Day3.Symbol(Day3.Position(21,5),'*'))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(32,5),3,988))
        yield Day3.SymbolChoice(Day3.Symbol(Day3.Position(43,5),'$'))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(46,5),3,241))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(51,5),3,750))
        yield Day3.SymbolChoice(Day3.Symbol(Day3.Position(67,5),'@'))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(80,5),3,169))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(86,5),3,162))
        yield Day3.SymbolChoice(Day3.Symbol(Day3.Position(95,5),'*'))
        yield Day3.SymbolChoice(Day3.Symbol(Day3.Position(99,5),'%'))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(104,5),3,575))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(108,5),3,743))
        yield Day3.SymbolChoice(Day3.Symbol(Day3.Position(117,5),'*'))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(136,5),3,278))
    }
    Assert.Equal(expected |> Seq.toList, result)
    
[<Fact>]
[<Trait("Category","Day 3")>]
let ``Get Symbols And Numbers From line2`` () =
    let result = Day3.GetSymbolsAndNumbersFromLine "..863.%..920..*152..852*387..990..563.*.*.629..772.912.283." 5
    let expected = seq {
        yield Day3.NumberChoice(Day3.Number(Day3.Position(2,5),3,863))
        yield Day3.SymbolChoice(Day3.Symbol(Day3.Position(6,5),'%'))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(9,5),3,920))
        yield Day3.SymbolChoice(Day3.Symbol(Day3.Position(14,5),'*'))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(15,5),3,152))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(20,5),3,852))
        yield Day3.SymbolChoice(Day3.Symbol(Day3.Position(23,5),'*'))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(24,5),3,387))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(29,5),3,990))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(34,5),3,563))
        yield Day3.SymbolChoice(Day3.Symbol(Day3.Position(38,5),'*'))
        yield Day3.SymbolChoice(Day3.Symbol(Day3.Position(40,5),'*'))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(42,5),3,629))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(47,5),3,772))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(51,5),3,912))
        yield Day3.NumberChoice(Day3.Number(Day3.Position(55,5),3,283))
    }
    Assert.Equal(expected |> Seq.toList, result)
    
[<Fact>]
[<Trait("Category","Day 3")>]
let ``Get Symbols And Numbers From line3`` () =
    let result = Day3.GetSymbolsAndNumbersFromLine "4." 5
    let expected = seq {
        yield Day3.NumberChoice(Day3.Number(Day3.Position(0,5),1,4))
    }
    Assert.Equal(expected |> Seq.toList, result)
    
    
[<Fact>]
[<Trait("Category","Day 3")>]
let ``Get Symbols And Numbers From line5`` () =
    let result = Day3.GetSymbolsAndNumbersFromLine "4" 5
    let expected = seq {
        yield Day3.NumberChoice(Day3.Number(Day3.Position(0,5),1,4))
    }
    Assert.Equal(expected |> Seq.toList, result)

[<Fact>]
[<Trait("Category","Day 3")>]
let ``Get Symbols And Numbers From line4`` () =
    let result = Day3.GetSymbolsAndNumbersFromLine "%." 5
    let expected = seq {
        yield Day3.SymbolChoice(Day3.Symbol(Day3.Position(0,5),'%'))
    }
    Assert.Equal(expected |> Seq.toList, result)

//======================================= Day 4 ==============================

[<Fact>]
[<Trait("Category","Day 4")>]
let ``Day 4 star 1 test Data`` () =
    let result = Day4.RunStarOne @".\ValidationData4-1.txt"
    Assert.Equal(13, result)
    

[<Fact>]
[<Trait("Category","Day 4")>]
let ``Day 4 star 2 test Data`` () =
    let result = Day4.RunStarTwo @".\ValidationData4-2.txt"
    Assert.Equal(60, result)
    
[<Fact>]
[<Trait("Category","Day 5")>]
let ``Day 5 star 1 test Data`` () =
    let result = Day5.RunStarOne @".\ValidationData5-1.txt"
    Assert.Equal(35 |> int64, result)
    

[<Fact>]
[<Trait("Category","Day 5")>]
let ``Day 5 star 2 test Data`` () =
    let result = Day5.RunStarTwo @".\ValidationData5-2.txt"
    Assert.Equal(46 |> int64, result)

[<Fact>]
[<Trait("Category","Day 5")>]    
let ``Day 5 test mapping is in range`` () =
    let mapping = Day5.Mapping(98, 50, 2)
    Assert.True(mapping.IsInRange 98)
    Assert.True(mapping.IsInRange 99)
    Assert.False(mapping.IsInRange 100)
    Assert.False(mapping.IsInRange 97)
    Assert.False(mapping.IsInRange 50)
    Assert.False(mapping.IsInRange 51)

[<Fact>]
[<Trait("Category","Day 5")>]    
let ``Day 5 test mapping map values`` () =
    let mapping1 = Day5.Mapping(98, 50, 2)
    let mapping2 = Day5.Mapping(50, 52, 48)
    // testing mapping 1
    Assert.Throws(fun () ->
        mapping1.MapValue(5)|> ignore) |> ignore
    Assert.Equal(50|>int64,mapping1.MapValue 98)
    Assert.Equal(51|>int64,mapping1.MapValue 99)
    Assert.Throws(fun () ->
        mapping1.MapValue(97)|> ignore) |> ignore
    Assert.Throws(fun () ->
        mapping1.MapValue(100)|> ignore) |> ignore
    
    // testing mapping 2
    Assert.Throws(fun () ->
        mapping2.MapValue(5)|> ignore) |> ignore
    Assert.Equal(52|>int64,mapping2.MapValue 50)
    Assert.Equal(99|>int64,mapping2.MapValue 97)
    Assert.Throws(fun () ->
        mapping2.MapValue(98)|> ignore) |> ignore
    Assert.Throws(fun () ->
        mapping2.MapValue(49)|> ignore) |> ignore
    
[<Fact>]
[<Trait("Category","Day 5")>]    
let ``Day 5 use mapping to convert numbers`` () =
    let maps = seq {
        yield Day5.Mapping(98, 50, 2)
        yield Day5.Mapping(50, 52, 48)
    }
    Assert.Equal(81|>int64,Day5.ConvertNumberUsingMappings maps 79)
    Assert.Equal(14|>int64,Day5.ConvertNumberUsingMappings maps 14)
    Assert.Equal(57|>int64,Day5.ConvertNumberUsingMappings maps 55)
    Assert.Equal(13|>int64,Day5.ConvertNumberUsingMappings maps 13)
    Assert.Equal(51|>int64,Day5.ConvertNumberUsingMappings maps 99)
    
[<Fact>]
[<Trait("Category","Day 5")>]    
let ``Day 5 read mappings`` () =
    let lines = seq {
        yield "50 98 2"
        yield "52 50 48"
    }
    
    let expectedMappings = seq {
        yield Day5.Mapping(98, 50, 2)
        yield Day5.Mapping(50, 52, 48)
    }
    Assert.Equal(expectedMappings|> Seq.toList, Day5.ReadMapping (lines |> Seq.toArray))
    
[<Fact>]
[<Trait("Category","Day 5")>]    
let ``Day 5 test mapping mapping overlaps`` () =
    let mapping = Day5.Mapping(98, 50, 2)
    Assert.Equal(Day5.Overlap.NoOverlap,mapping.RangeOverlapsMapping (10,15))
    Assert.Equal(Day5.Overlap.Contained,mapping.RangeOverlapsMapping (98,99))
    Assert.Equal(Day5.Overlap.OverlapStart,mapping.RangeOverlapsMapping (95,98))
    Assert.Equal(Day5.Overlap.OverlapEnd,mapping.RangeOverlapsMapping (99,120))
    
[<Fact>]
[<Trait("Category","Day 5")>]    
let ``Day 5 Map ranges Contained`` () =
    let mapping = Day5.Mapping(50, 52, 48)
    let inputRange = (int64(79),int64(93))
    let expected = seq{
        (int64(81),int64(95))
    }
    
    let result = Day5.TransformRangeWithMapping inputRange mapping
    (expected, result) ||> Seq.iter2(fun x y -> Assert.Equal(x,y) )

[<Fact>]
[<Trait("Category","Day 5")>]    
let ``Day 5 Map ranges End Overlaps`` () =
    let mapping = Day5.Mapping(50, 52, 48)
    let inputRange = (int64(52),int64(99))
    let expected = seq{
        (int64(54),int64(99))
        (int64(98),int64(99))
    }
    
    let result = Day5.TransformRangeWithMapping inputRange mapping
    (expected, result) ||> Seq.iter2(fun x y -> Assert.Equal(x,y) )
    
[<Fact>]
[<Trait("Category","Day 5")>]    
let ``Day 5 Map ranges Start Overlaps`` () =
    let mapping = Day5.Mapping(50, 52, 48)
    let inputRange = (int64(45),int64(60))
    let expected = seq{
        (int64(45),int64(49))
        (int64(52),int64(62))
    }
    
    let result = Day5.TransformRangeWithMapping inputRange mapping
    (expected, result) ||> Seq.iter2(fun x y -> Assert.Equal(x,y) )
    
[<Fact>]
[<Trait("Category","Day 5")>]    
let ``Day 5 Map ranges Encompasses`` () =
    let mapping = Day5.Mapping(50, 52, 48)
    let inputRange = (int64(45),int64(99))
    let expected = seq{
        (int64(45),int64(49))
        (int64(52),int64(99))
        (int64(98),int64(99))
    }
    
    let result = Day5.TransformRangeWithMapping inputRange mapping
    (expected, result) ||> Seq.iter2(fun x y -> Assert.Equal(x,y) )
    
[<Fact>]
[<Trait("Category","Day 5")>]    
let ``Day 5 Map ranges multipleMappings`` () =
    let mappings =
        seq{
            seq{
                Day5.Mapping(98, 50, 2)
                Day5.Mapping(50, 52, 48)
            }
        }
    let inputRange = (int64(79),int64(99))
    let expected = seq{
        (int64(50),int64(51))
        (int64(81),int64(97))
    }
    
    let result = Day5.TransformRangeWithMappings inputRange mappings
    (expected, result) ||> Seq.iter2(fun x y -> Assert.Equal(x,y) )
    

[<Fact>]
[<Trait("Category","Day 6")>]
let ``Day 6 star 1 test Data`` () =
    let result = Day6.RunStarOne @".\ValidationData6-1.txt"
    Assert.Equal(int64(288), result)
    

[<Fact>]
[<Trait("Category","Day 6")>]
let ``Day 6 star 2 test Data`` () =
    let result = Day6.RunStarTwo @".\ValidationData6-2.txt"
    Assert.Equal(int64(71503), result)

[<Fact>]
[<Trait("Category","Day 7")>]
let ``Day 7 star 1 test Data`` () =
    let result = Day7.RunStarOne @".\ValidationData7-1.txt"
    Assert.Equal(6440, result)
    

[<Fact>]
[<Trait("Category","Day 7")>]
let ``Day 7 star 2 test Data`` () =
    let result = Day7.RunStarTwo @".\ValidationData7-2.txt"
    Assert.Equal(5905, result)
    
[<Fact>]
[<Trait("Category","Day 7")>]
let ``Day 7 star 1 test get absoluteRank`` () =
    let hand = Day7.Hand(0,"AAAAA")
    Assert.Equal(7270126uL, hand.GetAbsoluteRank)

[<Fact>]
[<Trait("Category","Day 7")>]
let ``Day 7 star 1 test get absoluteRank2`` () =
    let hand = Day7.Hand(0,"23456")
    Assert.Equal(144470uL, hand.GetAbsoluteRank)

[<Fact>]
[<Trait("Category","Day 7")>]
let ``Day 7 star 1 test get Hand types jokers works bases`` () =
    let hand = Day7.HandWithJoker(0,"J3456")
    Assert.Equal(Day7.HandType.OnePair, hand.GetHandType)
    let hand2 = Day7.HandWithJoker(0,"JJ456")
    Assert.Equal(Day7.HandType.ThreeOfAKind, hand2.GetHandType)
    let hand3 = Day7.HandWithJoker(0,"JJJ56")
    Assert.Equal(Day7.HandType.FourOfAKind, hand3.GetHandType)
    let hand4 = Day7.HandWithJoker(0,"JJJJ6")
    Assert.Equal(Day7.HandType.FiveOfAKind, hand4.GetHandType)
    
[<Fact>]
[<Trait("Category","Day 7")>]
let ``Day 7 star 1 test get Hand types jokers works FiveOfAKind`` () =
    let hand = Day7.HandWithJoker(0,"AAAAJ")
    Assert.Equal(Day7.HandType.FiveOfAKind, hand.GetHandType)
    let hand2 = Day7.HandWithJoker(0,"AAAJJ")
    Assert.Equal(Day7.HandType.FiveOfAKind, hand2.GetHandType)
    let hand3 = Day7.HandWithJoker(0,"AAJJJ")
    Assert.Equal(Day7.HandType.FiveOfAKind, hand3.GetHandType)
    
[<Fact>]
[<Trait("Category","Day 7")>]
let ``Day 7 star 1 test get Hand types jokers works FourOfAKind`` () =
    let hand = Day7.HandWithJoker(0,"AAAJ2")
    Assert.Equal(Day7.HandType.FourOfAKind, hand.GetHandType)
    let hand2 = Day7.HandWithJoker(0,"2AAAJ")
    Assert.Equal(Day7.HandType.FourOfAKind, hand2.GetHandType)
    let hand3 = Day7.HandWithJoker(0,"2AAJJ")
    Assert.Equal(Day7.HandType.FourOfAKind, hand3.GetHandType)
    let hand3 = Day7.HandWithJoker(0,"2JJJA")
    Assert.Equal(Day7.HandType.FourOfAKind, hand3.GetHandType)
    let hand3 = Day7.HandWithJoker(0,"JJJ2A")
    Assert.Equal(Day7.HandType.FourOfAKind, hand3.GetHandType)
    let hand3 = Day7.HandWithJoker(0,"JJA33")
    Assert.Equal(Day7.HandType.FourOfAKind, hand3.GetHandType)

[<Fact>]
[<Trait("Category","Day 7")>]
let ``Day 7 star 1 test get Hand types jokers works TreeOfAKind`` () =
    let hand = Day7.HandWithJoker(0,"AAJ32")
    Assert.Equal(Day7.HandType.ThreeOfAKind, hand.GetHandType)
    let hand2 = Day7.HandWithJoker(0,"JAA23")
    Assert.Equal(Day7.HandType.ThreeOfAKind, hand2.GetHandType)
    let hand3 = Day7.HandWithJoker(0,"J23JA")
    Assert.Equal(Day7.HandType.ThreeOfAKind, hand3.GetHandType)
    
[<Fact>]
[<Trait("Category","Day 7")>]
let ``Day 7 star 1 test get Hand types jokers works FullHouse`` () =
    let hand = Day7.HandWithJoker(0,"AAJ22")
    Assert.Equal(Day7.HandType.FullHouse, hand.GetHandType)
    let hand2 = Day7.HandWithJoker(0,"JA3A3")
    Assert.Equal(Day7.HandType.FullHouse, hand2.GetHandType)
    
    
[<Fact>]
[<Trait("Category","Day 8")>]
let ``Day 8 star 1 test Data`` () =
    let result = Day8.RunStarOne @".\ValidationData8-1.txt"
    Assert.Equal(2uL, result)
    
[<Fact>]
[<Trait("Category","Day 8")>]
let ``Day 8 star 1 test Data 2`` () =
    let result = Day8.RunStarOne @".\ValidationData8-1.2.txt"
    Assert.Equal(6uL, result)
    

[<Fact>]
[<Trait("Category","Day 8")>]
let ``Day 8 star 2 test Data`` () =
    let result = Day8.RunStarTwo @".\ValidationData8-2.txt"
    Assert.Equal(6uL, result)
    
[<Fact>]
[<Trait("Category","Day 8")>]
let ``Day 8 test direction sequence works`` () =
    let result = Day8.DirectionSequence("RLRRLRLL")
    let repeatedSequenceArray  =  result |> Seq.take(16) |> Seq.toArray
    Assert.Equal(Day8.Direction.Right, repeatedSequenceArray.[0])
    Assert.Equal(Day8.Direction.Left, repeatedSequenceArray.[1])
    Assert.Equal(Day8.Direction.Right, repeatedSequenceArray.[2])
    Assert.Equal(Day8.Direction.Right, repeatedSequenceArray.[3])
    Assert.Equal(Day8.Direction.Left, repeatedSequenceArray.[4])
    Assert.Equal(Day8.Direction.Right, repeatedSequenceArray.[5])
    Assert.Equal(Day8.Direction.Left, repeatedSequenceArray.[6])
    Assert.Equal(Day8.Direction.Left, repeatedSequenceArray.[7])
    Assert.Equal(Day8.Direction.Right, repeatedSequenceArray.[8])
    Assert.Equal(Day8.Direction.Left, repeatedSequenceArray.[9])
    Assert.Equal(Day8.Direction.Right, repeatedSequenceArray.[10])
    Assert.Equal(Day8.Direction.Right, repeatedSequenceArray.[11])
    Assert.Equal(Day8.Direction.Left, repeatedSequenceArray.[12])
    Assert.Equal(Day8.Direction.Right, repeatedSequenceArray.[13])
    Assert.Equal(Day8.Direction.Left, repeatedSequenceArray.[14])
    Assert.Equal(Day8.Direction.Left, repeatedSequenceArray.[15])
    
    
    
[<Fact>]
[<Trait("Category","Day 9")>]
let ``Day 9 star 1 test Data`` () =
    let result = Day9.RunStarOne @".\ValidationData9-1.txt"
    Assert.Equal(114L, result)
    

[<Fact>]
[<Trait("Category","Day 9")>]
let ``Day 9 star 2 test Data`` () =
    let result = Day9.RunStarTwo @".\ValidationData9-2.txt"
    Assert.Equal(2L, result)
    

[<Theory>]
[<InlineData("0 3 6 9 12 15",18)>]
[<InlineData("1 3 6 10 15 21",28)>]
[<InlineData("10 13 16 21 30 45",68)>]
[<Trait("Category","Day 9")>]
let ``Day 9 Get line's next number`` (input:string, expected:int64)  =
    let result = Day9.GetLineNextNumber input
    Assert.Equal(expected, result)
    
[<Fact>]
[<Trait("Category","Day 9")>]
let ``Day 9 Get diff array`` ()  =
    let result = Day9.GetDiffArray [|0;3;6;9;12;15|]
    let expected = [|3L;3L;3L;3L;3L|]
    Assert.True(expected |> Array.forall2(=) result)
    
[<Fact>]
[<Trait("Category","Day 9")>]
let ``Day 9 Get diff array 2`` ()  =
    let result = Day9.GetDiffArray [|2;3;4;5;6|]
    let expected = [|1L;1L;1L;1L|]
    Assert.True(expected |> Array.forall2(=) result)
    
[<Fact>]
[<Trait("Category","Day 9")>]
let ``Day 9 Get diff array negative 1`` ()  =
    let result = Day9.GetDiffArray [|-6;-5;-4;-3;-2|]
    let expected = [|1L;1L;1L;1L|]
    Assert.True(expected |> Array.forall2(=) result)
    
[<Fact>]
[<Trait("Category","Day 9")>]
let ``Day 9 Get diff array negative 2`` ()  =
    let result = Day9.GetDiffArray [|-2;-3;-4;-5;-6|]
    let expected = [|-1L;-1L;-1L;-1L|]
    Assert.True(expected |> Array.forall2(=) result)
    
    
[<Fact>]
[<Trait("Category","Day 10")>]
let ``Day 10 star 1 test Data`` () =
    let result = Day10.RunStarOne @".\ValidationData10-1.1.txt"
    Assert.Equal(4, result)
    
[<Fact>]
[<Trait("Category","Day 10")>]
let ``Day 10 star 1 test Data2`` () =
    let result = Day10.RunStarOne @".\ValidationData10-1.2.txt"
    Assert.Equal(4, result)
    
[<Fact>]
[<Trait("Category","Day 10")>]
let ``Day 10 star 1 test Data3`` () =
    let result = Day10.RunStarOne @".\ValidationData10-1.3.txt"
    Assert.Equal(8, result)
    
[<Fact>]
[<Trait("Category","Day 10")>]
let ``Day 10 star 1 test Data4`` () =
    let result = Day10.RunStarOne @".\ValidationData10-1.4.txt"
    Assert.Equal(8, result)
    

[<Fact>]
[<Trait("Category","Day 10")>]
let ``Day 10 star 2 test Data`` () =
    let result = Day10.RunStarTwo @".\ValidationData10-2.txt"
    Assert.Equal(0, result)
    
    
    
[<Fact>]
[<Trait("Category","Day 11")>]
let ``Day 11 star 1 test Data`` () =
    let result = Day11.RunStarOne @".\ValidationData10-1.txt"
    Assert.Equal(0, result)
    

[<Fact>]
[<Trait("Category","Day 11")>]
let ``Day 11 star 2 test Data`` () =
    let result = Day11.RunStarTwo @".\ValidationData10-2.txt"
    Assert.Equal(0, result)
    
