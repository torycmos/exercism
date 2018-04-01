module Accumulate

//guess I shouldn't be quite so lazy
let accumulate f xs =
    let rec loop acc f xs =
        match xs with
        | [] -> acc |> List.rev
        | x::xs' -> loop (f x::acc) f xs'

    loop [] f xs