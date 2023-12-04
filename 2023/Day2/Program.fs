open System
open System.IO
open System.Collections.Generic

module Day2 =
    
    type Game =
        struct
            val Index : int
            val NbRed : int
            val NbGreen : int
            val NbBlue : int
            new (index: int) = {Index = index; NbRed = 0; NbGreen=0; NbBlue = 0}
            new (index: int, red:int, green:int, blue:int) = {Index = index; NbRed = red; NbGreen=green; NbBlue = blue}
            
            member this.IsPossible(red:int, green:int, blue:int) : bool =
                 (red >= this.NbRed && green >= this.NbGreen && blue >= this.NbBlue)
        end
    
    let ReadData (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    
    
    let CreateGame (gameStr:string) (index:int) : Game =
        
        let dict = new Dictionary<string, int>()
        gameStr.Split(",") |> Array.iter(fun x ->
                                                let g = x.Trim().Split(" ")
                                                let amount = g[0] |> int
                                                dict.Add(g[1],amount)
                                                )
        Game(index, dict.GetValueOrDefault("red"), dict.GetValueOrDefault("green"), dict.GetValueOrDefault("blue"))
    
    let GetGamesFromString(gameSeries:string) : seq<Game> =
        let splitGameIndex = gameSeries.Split(":")
        let gameIndex = splitGameIndex[0].Split(" ")[1] |> int
        splitGameIndex[1].Split(";")
                    |> Array.map (fun x -> CreateGame x gameIndex)
                    |> Array.toSeq
        
        
    
    let RunStarOne (filePath:string) : int =
        let data = ReadData filePath
        let games = data    |> Seq.map GetGamesFromString
                          
        let validGameIndices = games |> Seq.map(fun x ->
                                                    let isPossible = x
                                                                     |> Seq.forall(fun g -> g.IsPossible(12,13,14))
                                                    match isPossible with
                                                    | true ->   let g = x |> Seq.head
                                                                g.Index
                                                    | false -> 0)
        validGameIndices |> Seq.sum
    
    let GetGameSeriesPower (gameSeries:seq<Game>) : int =
        let reds = gameSeries |> Seq.map(fun x -> x.NbRed) |> Seq.max
        let greens = gameSeries |> Seq.map(fun x -> x.NbGreen) |> Seq.max
        let blues = gameSeries |> Seq.map(fun x -> x.NbBlue) |> Seq.max
        reds * greens * blues
        
    let RunStarTwo (filePath:string) : int =
        let data = ReadData filePath
        let games = data    |> Seq.map GetGamesFromString
                          
        let validGameIndices = games |> Seq.map(GetGameSeriesPower)  
        validGameIndices |> Seq.sum 
        
    

Day2.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day2.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"
    