open System
open System.Diagnostics.Contracts
open System.IO
open System.Collections.Generic

module Day3 =
    
    type Position =
        struct
            val X : int
            val Y : int
            new (x:int, y : int) = {X = x; Y = y;}
        end
    
    type Symbol =
        struct
            val Position : Position
            val Value : char
            new (position:Position, value : char) = {Position = position; Value = value;}
        end
    
    type Number =
        struct
            val Position : Position
            val Size : int
            val Value : int
            new (position:Position, size: int, value : int) = {Position = position; Size = size; Value = value;}
            
            member this.IsAdjacentToSymbols (symbols:seq<Symbol>) : bool =
                
                let maxX = this.Position.X + this.Size
                let minX = this.Position.X - 1
                let maxY = this.Position.Y + 1
                let minY = this.Position.Y - 1;
                let foundSymbol = symbols |> Seq.tryPick(fun s ->
                                                            if s.Position.X >= minX
                                                               && s.Position.X <= maxX
                                                               && s.Position.Y >= minY
                                                               && s.Position.Y <= maxY then
                                                                Some(s)
                                                             else
                                                                None
                                                            )
                match foundSymbol with
                | Some _ -> true
                | None -> false
        end
    
    
    
    type NumberOrSymbol =
        | NumberChoice of Number
        | SymbolChoice of Symbol
    
    let ReadData (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    
    let GetSymbolsAndNumbersFromLine (line:string) (lineNb:int) : seq<NumberOrSymbol> = seq{
        // Note, find a way to do this without being mutable
        let mutable numberBuffer = String.Empty
        let mutable numberPosition:Option<Position> = None
        for i in [0..line.Length-1] do
            if line[i] = '.' then
                if not(numberBuffer.Equals(String.Empty))  then
                    match numberPosition with
                    | None -> failwith "no position set for number"
                    | Some(numberPosition) -> yield NumberChoice(Number(numberPosition, numberBuffer.Length , numberBuffer |> int))
                    numberBuffer <- String.Empty
                    numberPosition <- None
            elif Char.IsNumber line[i] then
                numberBuffer <- numberBuffer.Insert(numberBuffer.Length, line[i] |> string )
                numberPosition <- match numberPosition with
                                    | None -> Some(Position(i, lineNb))
                                    | Some(numberPosition) -> Some(numberPosition)
            else
                if not(numberBuffer.Equals(String.Empty))  then
                    match numberPosition with
                    | None -> failwith "no position set for number"
                    | Some(numberPosition) -> yield NumberChoice(Number(numberPosition, numberBuffer.Length , numberBuffer |> int))
                    numberBuffer <- String.Empty
                    numberPosition <- None
                yield SymbolChoice(Symbol(Position(i, lineNb), line[i]))
                
        if not(numberBuffer.Equals(String.Empty))  then
            match numberPosition with
            | None -> failwith "no position set for number"
            | Some(numberPosition) -> yield NumberChoice(Number(numberPosition, numberBuffer.Length , numberBuffer |> int))
            
    }
    
    let FindAdjacentNumbers2 (symbolsAndNumbers:seq<NumberOrSymbol>) : seq<int> =
        let symbols = symbolsAndNumbers |> Seq.choose(fun x -> match x with
                                                                | SymbolChoice x -> Some(x)
                                                                | NumberChoice _ -> None)
                                        |> Seq.cache // cache it since we will iterate through it many times
        let numbers = symbolsAndNumbers |> Seq.choose(fun x -> match x with
                                                                | SymbolChoice _ -> None
                                                                | NumberChoice x -> Some(x))
        numbers |> Seq.where(fun x -> x.IsAdjacentToSymbols(symbols))
                |> Seq.map( fun x -> x.Value)
    
    let FindAdjacentNumbers (lines:seq<string>) : seq<int> =
        let symbolsAndNumbers = lines |> Seq.mapi(fun i l -> GetSymbolsAndNumbersFromLine l i) |> Seq.concat
        FindAdjacentNumbers2 symbolsAndNumbers
    let RunStarOne (filePath:string) : int =
        ReadData filePath |> FindAdjacentNumbers |> Seq.sum
        
    let FindGearRatios (symbolsAndNumbers:seq<NumberOrSymbol>) : seq<int> =
        let starSymbols = symbolsAndNumbers |> Seq.choose(fun x -> match x with
                                                                    | SymbolChoice x when x.Value.Equals('*') -> Some(x)
                                                                    | SymbolChoice _ -> None
                                                                    | NumberChoice _ -> None)
        let numbers = symbolsAndNumbers |> Seq.choose(fun x -> match x with
                                                                | SymbolChoice _ -> None
                                                                | NumberChoice x -> Some(x))
                                        |> Seq.cache // cache it since we will iterate through it many times
        // gears are adjacent to two numbers                                
        starSymbols |> Seq.choose(fun x ->
                                    let adjacentNumbers = numbers |> Seq.where(fun n -> n.IsAdjacentToSymbols(seq{yield x})) |> Seq.toArray
                                    if adjacentNumbers.Length = 2 then
                                        let gearRatio = adjacentNumbers[0].Value * adjacentNumbers[1].Value
                                        Some(gearRatio)
                                    else
                                        None
                                    )
        
    let RunStarTwo (filePath:string) : int =
        let symbolsAndNumbers = ReadData filePath |> Seq.mapi(fun i l -> GetSymbolsAndNumbersFromLine l i) |> Seq.concat
        FindGearRatios symbolsAndNumbers |> Seq.sum
        
Day3.RunStarOne @".\MyData.txt" |> printfn "Star One [%i]"
Day3.RunStarTwo @".\MyData.txt" |> printfn "Star Two [%i]"
    