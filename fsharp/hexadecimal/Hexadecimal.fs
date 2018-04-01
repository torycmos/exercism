module Hexadecimal
open System
open System.Text.RegularExpressions

let getValue (x : char) = 
    (['0'..'9']@['a'..'f'], [0 .. 15])
    ||> List.zip
    |> Map.ofList
    |> Map.find (Char.ToLowerInvariant x)

let toDecimal str =
    if Regex.Match(str,"[^0-9a-fA-F]").Success then 0
    else str.ToCharArray()
         |> Array.rev
         |> Array.mapi (fun n x -> (getValue x) * (pown 16 n))
         |> Array.sum
         