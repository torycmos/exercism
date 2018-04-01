module Allergies

open System

[<Flags>]  // now with less bit-janitoring
type Allergen = 
    | Eggs = 1
    | Peanuts = 2
    | Shellfish = 4
    | Strawberries = 8
    | Tomatoes = 16
    | Chocolate = 32
    | Pollen = 64
    | Cats = 128

let allergicTo allergen testVal = 
    (enum<Allergen>testVal).HasFlag(allergen)

let allergies (n : int) = 
    [0..7] |> List.map (pown 2)
    |> Seq.cast<Allergen>
    |> Seq.filter (fun x -> allergicTo x n)