open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

module Day8 =
    
    let ReadData (filePath:string) : seq<string> = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    
    type Direction =
        | Left = 0
        | Right = 1
    
    let DirectionSequence(input:string) : seq<Direction> =
        let charNumber = input.Length
        seq{
            while true do
                let indices = seq { 0..charNumber-1 }
                for  i in indices do
                    yield match input.Chars(i) with
                            |'R' -> Direction.Right
                            |'L' -> Direction.Left
                            |_ -> failwith "Unknown direction"
        }
        
    let GetMaps(input:seq<string>) : Dictionary<string, string*string> =
        let dict = new Dictionary<string, string*string>()
        let rx = Regex(@"[A-Z]{3}", RegexOptions.Compiled)
        for l in input do
            let matches = rx.Matches(l)
            dict.Add(matches.Item(0).Value,(matches.Item(1).Value,matches.Item(2).Value))
        dict
    
    let RunStarOne (filePath:string) : int =
        let directions = ReadData filePath |> Seq.head
        let directionsSequence = DirectionSequence directions
        let map = ReadData filePath |> Seq.skip(2) |> GetMaps
        let mutable count = 0
        let mutable currentPosition = "AAA"
        let mutable foundZZZ = false
        let enumerator = directionsSequence.GetEnumerator()
        while not foundZZZ do
            count <- count + 1
            let (left, right) = map.Item currentPosition
            if enumerator.MoveNext() then
                currentPosition <- match enumerator.Current with
                                    | Direction.Left -> left
                                    | Direction.Right -> right
                                    | _ -> failwith "unknown direction"
            else
                failwith "no more sequence"
            foundZZZ <- currentPosition = "ZZZ"
                
            
        count
            
    
    let RunStarTwo (filePath:string) : int = 0 
        
    

Day8.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day8.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"
    