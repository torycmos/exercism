module LargestSeriesProduct

let largestProduct (digits : string) size = 
    match digits.Length, size with
    | 0, x when x > 0 -> invalidOp "empty string with nonzero span"
    | _, 0 -> 1
    | _, x ->
        digits.ToCharArray() 
        |> Array.map (fun x -> int x - int '0')
        |> Array.windowed x
        |> Array.map (fun x -> Array.reduce (*) x)
        |> Array.max