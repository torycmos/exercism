module Scrabble
open System
open ETL

let scores =
    [(0, [" "; "\n"; "\t"])
     (1, ["A"; "E"; "I"; "O"; "U"; "L"; "N"; "R"; "S"; "T"]) 
     (2, ["D"; "G"]) 
     (3, ["B"; "C"; "M"; "P"]) 
     (4, ["F"; "H"; "V"; "W"; "Y"]) 
     (5, ["K"]) 
     (8, ["J"; "X"]) 
     (10, ["Q"; "Z"])] 
    |> Map.ofSeq
    |> ETL.transform

let score (str : string) = 
    str.ToLowerInvariant()
    |> Seq.chunkBySize 1 
    |> Seq.map (fun x -> scores.Item(String x))
    |> Seq.sum