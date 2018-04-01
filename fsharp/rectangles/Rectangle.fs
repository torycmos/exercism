module Rectangle

// look at a given string and find any vertex with a valid connection to the starting point
let findVertices (row : string) = 
    let rec contig acc i (xs : char list) =
        match xs with
        | x::xs' -> match x with
                    | '-' -> contig acc (i + 1) xs'
                    | '+' -> contig (i::acc) (i + 1) xs'
                    | _ -> List.rev acc
        | [] -> List.rev acc
    contig [] 1 (row |> List.ofSeq)
    
// find all the valid horizontal parts of rectangles present in a row as a list of tuples
// containing the column indexes of the re
let parseRow (row : string) =
  row
  |> Seq.indexed 
  |> Seq.collect (fun (x,y) -> 
    match y with
    //at each index with a vertex, call findVertices to get a list of valid vertices starting
    //from that index in the string; 
    | '+' -> findVertices (row.Substring(x+1))
               |> List.map (fun vertex -> (x,vertex + x))
    | _ -> []) 

let parseColumns =
  List.map parseRow
  >> List.indexed
  >> List.collect (fun (i,xs) -> xs
                                 |> Seq.map (fun x -> (i,x))
                                 |> List.ofSeq )
  >> List.groupBy (snd)
  >> List.filter (fun (x,y) -> y.Length > 1)
  >> List.map (fun x ->                   // list is (vertex cols)*(row * vertex cols list) now, so we
       (fst x), List.map (fst) (snd x))   // can keep just the pair of vertex col indexes and what rows
                                          // they're in
  
// determine if a pair of vertex col indexes in rows top and bot are connected
let rowConnect top bot (vertices : int * int) (rows : string list) =
  [for row in rows.[top..bot] ->
     row.[fst vertices]::[row.[snd vertices]]]
  |> List.concat
  |> List.exists (fun x -> x <> '|' && x <> '+') // if a column index doesn't have a pipe or vertex
  |> not                                         // then it can't connect different rows

// get the possible ordered pairs from a list like [1;2;3] -> (1,2),(1,3),(2,3)    
let rec combos = function
  | [] -> []
  | x::xs -> List.collect (fun xs -> [for x' in xs -> (x,x')]::(combos xs)) [xs]

let rectangles rows = 
  parseColumns rows
  |> List.collect (fun x -> 
       combos (snd x)
       |> List.concat
       |> List.map (fun y ->
            match rowConnect (fst y) (snd y) (fst x) rows with
            | true -> Some (fst x, y)
            | false -> None
          )
       )
  |> List.filter Option.isSome
  |> List.length