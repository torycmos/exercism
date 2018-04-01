module Acronym
open System
open System.Text
// dont consider consecutive caps, but do get other ones later in word
let capFilter str = str |> Seq.skipWhile (Char.IsUpper)
                        |> Seq.filter (Char.IsUpper)
                        |> String.Concat

let rec parseStr (acc : StringBuilder) (str : string) = 
    if str.Length > 0 
    then parseStr (acc.Append(str.Substring(0,1))) 
                  (str.Substring(1) |> capFilter)
    else acc.ToString()

let acronym (phrase : string) =
    phrase.Split([|' ';'-'|])
    |> Array.map (fun x -> parseStr (new StringBuilder()) x) 
    |> String.Concat
    |> (fun x -> x.ToUpperInvariant())