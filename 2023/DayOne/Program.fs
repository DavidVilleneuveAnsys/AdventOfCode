open System
open System.IO
open System.Text
open Microsoft.FSharp.Core

let readLines (filePath:string) = seq<string> {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let getIndexOfNumber (stringWithNumbers:string) (searchedString:string, replacement:string) : seq<int> * string =
    let indices = -1 |> Seq.unfold(fun x ->
        let i = stringWithNumbers.IndexOf(searchedString,x+1)
        match i with
        | i when i < 0 -> None
        | i -> Some(i,i)
    )
    (indices, replacement)
        
let textToNumberMap = ["one", "1";
                       "two", "2";
                       "three", "3";
                       "four", "4";
                       "five", "5";
                       "six", "6";
                       "seven", "7";
                       "eight", "8";
                       "nine", "9"] |> Map.ofList

let replaceNumbersInString(stringWithNumbers:string) : string =
    let indices = textToNumberMap   |> Map.toSeq
                                    |> Seq.map(getIndexOfNumber stringWithNumbers)
                                    
    (stringWithNumbers, indices)
    ||> Seq.fold(fun acc i ->
                        let index,replacement = i
                        match index with
                        | x when Seq.isEmpty x -> acc
                        | x -> (acc, x)
                            ||> Seq.fold(
                                fun a j ->
                                    a.Remove(j,1).Insert(j,replacement))
                        )
let getOnlyInts(line:string) : string =
    let i = new StringBuilder()
    
    let replacedString = replaceNumbersInString line
    replacedString
    |> String.iter(fun x ->
                    match x with
                    | x when Char.IsNumber x -> i.Append x
                                                |> ignore
                    | _ -> ())
    
    let result = i.ToString()
    result

let getFirstAndLastChar(number:string) : string =
   let i = new StringBuilder()
   i.Append number[0] |> ignore
   i.Append number[number.Length-1] |> ignore
   let result = i.ToString()
   result

let getFirstAndLastIntsFromLine(line:string) : int =
    getOnlyInts line
    |> getFirstAndLastChar
    |> int
    
let mySeq = readLines @".\day1Input2.txt"
mySeq
|> Seq.map getFirstAndLastIntsFromLine
|> Seq.sum
|> printf "%i"