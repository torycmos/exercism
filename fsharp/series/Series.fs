module Series

let charToInt x = int x - int '0'

let slices (str : string) n =
    let intLst (xs : string) = 
        List.ofSeq xs
        |> List.map (charToInt)

    let rec loop acc n (xs : int list) =
        match xs.Length >= n with
        | true -> loop ((List.take n xs) :: acc) n (xs.Tail)
        | false -> List.rev acc

    if n > str.Length then 
       failwith "cootie vaccine gave me autism"
    else loop [] n (intLst str)