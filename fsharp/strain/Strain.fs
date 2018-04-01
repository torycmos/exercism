module Seq
open System
// this passes the tests though it doesn't keep the original collection type
let rec Filter (a,b) f xs k =
    match Seq.tryHead xs with
    | None -> k (a,b)
    | Some x -> 
        if f x then Filter (x::a,b) f (Seq.tail xs) k
        else Filter (a,x::b) f (Seq.tail xs) k

let keep f xs = Filter ([],[]) f (xs |> seq) (fst >> Seq.rev)  
let discard f xs = Filter ([],[]) f (xs |> seq) (snd >> Seq.rev)