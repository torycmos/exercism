module NthPrime
open System.Collections

// the actual prime getter I left out previously; saw this idea of using a bool bitarray
// with the indexes themselves being potential primes somewhere. The value at index n is set to false
// if n is a mulitple of the current (or any previous) prime seived out, so if index n is still true when
// the seive gets to it, it must not be a multiple of any previous primes and is thus prime itself
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

(*
 * uses a prime counting function approximation; the value at each index is the number 
 * of primes less than that index's value; e.g. at index 5 the value is 3 because [1;2;3] 
 * are primes less than 5, and at index 6 the value is 4 because the primes below it are [1;2;3;5]
 *
 * I use this as the constraint for how big a set of numbers to sieve the primesUpTo function gets.
 *) 
let primeCount = seq {
    for i in 1 .. System.Int32.MaxValue ->
    (float i) / (log (float i)) |> ceil |> int }

let primeIndex n =
    Seq.findIndex (fun x -> x = n) primeCount

// the n<100 cutoff was arbitrary; skips using the counting function just to avoid trouble at the low end
let nthPrime n =
    match n < 100 with
    | true -> primesUpTo 600
    | false -> primesUpTo (primeIndex n) 
    |> List.item (n-1)