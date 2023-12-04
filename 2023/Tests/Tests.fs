module Tests

open System
open Program
open Xunit

//===========================Day 2 ===============================
[<Fact>]
let ``Day 2 star 1 test Data`` () =
    let result = Day2.RunStarOne @".\ValidationData1.txt"
    Assert.Equal(8, result)

[<Fact>]
let ``Day 2 star 2 test Data`` () =
    let result = Day2.RunStarTwo @".\ValidationData2.txt"
    Assert.Equal(2286, result)

[<Fact>]
let ``Test architecture test`` () =
    Assert.All(Day2.ReadData @".\testData.txt"
               |> Seq.toList,
               fun x -> Assert.Equal(x,"test"))
    
[<Fact>]
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
let ``Day 3 star 1 test Data`` () =
    let result = Day3.RunStarOne @".\ValidationData3-1.txt"
    Assert.Equal(4361, result)
    
[<Fact>] 
let ``Day 3 star 1 test Data2`` () =
    let result = Day3.RunStarOne @".\ValidationData3-1.2.txt"
    Assert.Equal(15634, result)

[<Fact>] 
let ``Day 3 star 1 test Data3`` () =
    let result = Day3.RunStarOne @".\ValidationData3-1.3.txt"
    Assert.Equal(6463, result)

[<Fact>]    
let ``Day 3 star 2 test Data`` () =
    let result = Day3.RunStarTwo @".\ValidationData3-2.txt"
    Assert.Equal(467835, result)
    
[<Fact>]
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
let ``Get Symbols And Numbers From line3`` () =
    let result = Day3.GetSymbolsAndNumbersFromLine "4." 5
    let expected = seq {
        yield Day3.NumberChoice(Day3.Number(Day3.Position(0,5),1,4))
    }
    Assert.Equal(expected |> Seq.toList, result)
    
    
[<Fact>]
let ``Get Symbols And Numbers From line5`` () =
    let result = Day3.GetSymbolsAndNumbersFromLine "4" 5
    let expected = seq {
        yield Day3.NumberChoice(Day3.Number(Day3.Position(0,5),1,4))
    }
    Assert.Equal(expected |> Seq.toList, result)

[<Fact>]
let ``Get Symbols And Numbers From line4`` () =
    let result = Day3.GetSymbolsAndNumbersFromLine "%." 5
    let expected = seq {
        yield Day3.SymbolChoice(Day3.Symbol(Day3.Position(0,5),'%'))
    }
    Assert.Equal(expected |> Seq.toList, result)

//======================================= Day 4 ==============================

[<Fact>] 
let ``Day 4 star 1 test Data`` () =
    let result = Day4.RunStarOne @".\ValidationData4-1.txt"
    Assert.Equal(13, result)
    

[<Fact>] 
let ``Day 4 star 2 test Data`` () =
    let result = Day4.RunStarTwo @".\ValidationData4-2.txt"
    Assert.Equal(60, result)