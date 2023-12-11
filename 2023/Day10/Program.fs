open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

module Day10 =
    
    let ReadData (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    
    type Direction =
        | North = 0
        | South = 1
        | East = 2
        | West = 3
    
    let MoveNorth(currentPosition:int) (fieldSize:int): option<int> =
        let position = currentPosition - fieldSize
        if position >= 0 then
            Some(position)
        else
            None
        
    let MoveSouth(currentPosition:int) (fieldSize:int) (maxSize:int):option<int> =
        let position = currentPosition + fieldSize
        if position < maxSize then
            Some(position)
        else
            None
    
    let MoveEast(currentPosition:int) (fieldSize:int): option<int> =
        if not(currentPosition % (fieldSize + 1) = 0)  then 
            Some(currentPosition + 1)
        else
            None
            
        
    let MoveWest(currentPosition:int) (fieldSize:int) : option<int> =
        if not(currentPosition % (fieldSize - 1) = 0)  then 
            Some(currentPosition + 1)
        else
            None
            
    let IsValidDirection (currentPipe:char) (nextPipe:char) (movingDirection:Direction) : bool =
        match currentPipe with
        | 'S' when movingDirection = Direction.North && (nextPipe = '|' || nextPipe = '7' || nextPipe = 'F') -> true
        | 'S' when movingDirection = Direction.South && (nextPipe = '|' || nextPipe = 'L' || nextPipe = 'J') -> true
        | 'S' when movingDirection = Direction.East && (nextPipe = '-' || nextPipe = '7' || nextPipe = 'J') -> true
        | 'S' when movingDirection = Direction.West && (nextPipe = '-' || nextPipe = 'L' || nextPipe = 'F') -> true
        | '|' when movingDirection = Direction.North && (nextPipe = '|' || nextPipe = '7' || nextPipe = 'F')-> true
        | '|' when movingDirection = Direction.South && (nextPipe = '|' || nextPipe = 'L' || nextPipe = 'J')-> true
        | '-' when movingDirection = Direction.East && (nextPipe = '-' || nextPipe = '7' || nextPipe = 'J') -> true
        | '-' when movingDirection = Direction.West && (nextPipe = '-' || nextPipe = 'L' || nextPipe = 'F') -> true
        | 'L' when movingDirection = Direction.North && (nextPipe = '|' || nextPipe = '7' || nextPipe = 'F')-> true
        | 'L' when movingDirection = Direction.South && (nextPipe = '|' || nextPipe = 'L' || nextPipe = 'J')-> true
        | '-' when movingDirection = Direction.East && (nextPipe = '-' || nextPipe = '7' || nextPipe = 'J') -> true
        | '-' when movingDirection = Direction.West && (nextPipe = '-' || nextPipe = 'L' || nextPipe = 'F') -> true
        | _ -> false
        
    
    let RunStarOne (filePath:string) : int =
       let arrayOfStrings = ReadData filePath |> Seq.toArray
       let sizeOfField = arrayOfStrings |> Array.head |> String.length
       let pipeField = arrayOfStrings |> Array.map(fun x -> x.ToCharArray() ) |> Array.concat
       let startIndex = pipeField |> Array.findIndex(fun x -> x.Equals 'S')
       startIndex
       
    let RunStarTwo (filePath:string) : int = 0 
        
    

Day10.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day10.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"