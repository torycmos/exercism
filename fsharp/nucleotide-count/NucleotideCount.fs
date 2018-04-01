module NucleoTideCount
open System

let nucleotideCounts (str : string) =
    let folder (As,Ts,Cs,Gs) letter =
        match letter with
        |'A' -> As + 1,Ts,Cs,Gs
        |'T' -> As,Ts + 1,Cs,Gs
        |'C' -> As,Ts,Cs + 1,Gs
        |'G' -> As,Ts,Cs,Gs + 1
        |_ -> invalidOp "NaN"
    Array.fold folder (0,0,0,0) (str.ToUpperInvariant().ToCharArray())
    |> (fun (a,b,c,d) -> [('A',a);('T',b);('C',c);('G',d)])
    |> Map.ofList

let count letter str = 
    let tides = nucleotideCounts str
    match Map.tryFind letter tides with
        | Some x -> x
        | None -> failwith "NaN" 
    