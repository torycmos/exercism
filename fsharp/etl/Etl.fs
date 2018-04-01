module ETL

let mapper (x : int * string list) = 
    snd x 
    |> List.fold (fun acc y -> (y.ToLower(), fst x)::acc) [] 
let transform x = 
    Map.toList x
    |> List.collect mapper
    |> Map.ofList