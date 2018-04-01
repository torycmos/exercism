module CryptoSquare
open System

let normalizePlaintext (str : string) =
    str.ToCharArray()
    |> Array.filter Char.IsLetterOrDigit
    |> Array.map Char.ToLower
    |> String

let size =
    normalizePlaintext 
    >> String.length
    >> float >> sqrt >> ceil >> int

let plaintextSegments (str : string) =
    let txt = normalizePlaintext str 
    Seq.chunkBySize (size txt) txt
    |> Seq.map String
    |> List.ofSeq

let stringIndexes (str : string) = 
    str.ToCharArray()
    |> Array.mapi (fun index x -> (index, x))
    |> List.ofArray

let cipher k =
    plaintextSegments 
    >> List.map stringIndexes
    >> List.concat
    >> List.groupBy fst
    >> List.map snd
    >> List.map (List.fold (fun acc (x,y) -> acc + string y) "")
    >> k

let ciphertext = cipher (List.reduce (+))

let normalizeCiphertext = cipher (String.concat " ")