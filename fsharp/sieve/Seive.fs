module Sieve
open System.Collections

let primesUpTo n =
    let arr = new BitArray(n+1, true) //array of length n only goes to index n-1
    [2..n]
    |> List.map (fun x -> 
        if arr.Get(x) 
        then 
            [x*2 .. x .. n]
            |> List.map (fun x -> arr.Set(x,false)) |> ignore
            x
        else 0 )
    |> List.filter ((<>) 0)