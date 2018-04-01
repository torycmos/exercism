module Luhn
open System

//convert some int to its constituent digits
let getNums (x : int64) =
    x.ToString().ToCharArray()
    |> Array.map (fun x -> int x - int '0')

//was trying to get cute with point-free stuff but ended up
//needing more characters + being fairly confusing to boot
let (<.>) f a b = f b a
let checkDigit = ((%) <.> 10L) >> int
    
//get new digits with every other digit doubled. Map function operates
//on odd indices since the future checksum digit in the 1s place is included
let addends x =
    Array.rev (getNums x)
    |> Array.mapi (fun n x -> 
        match n % 2, x * 2 with
        | 0, _ -> x
        | _, y -> if y > 10 then y - 9 else y )
    |> Array.rev |> List.ofArray

//checksum digit is sum of individual addend digits % 10
let checksum = int64 >> addends >> List.sum >> (fun x -> x % 10)
let valid x = checksum x = 0

//this is janky due to the tests kind of randomly wanting int and int64
//if 10 * num doesn't produce a valid (0) checksum, using (10 - checksum) will
let makeLuhn n =
    let foo = addends (n * 10L) |> List.sum
    match foo % 10 with
    | 0 -> 0L
    | x -> (10 - x) |> int64
    
let create n = 
    (n * 10L) + makeLuhn n