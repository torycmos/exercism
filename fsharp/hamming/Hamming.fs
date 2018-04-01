module Hamming

let compute (x : string) (y : string) =
    (x.ToCharArray(), y.ToCharArray())
    ||> Array.fold2 (fun acc x y -> if x <> y then acc + 1
                                    else acc)
                    0