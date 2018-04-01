module Anagram

let sortWord (x : string) =
    x.ToLower().ToCharArray() |> Array.sort

let anagrams (xs : string list) (str : string) =
    List.filter (fun x -> sortWord x = sortWord str) xs
    |> List.filter (fun x -> x.ToLower().Equals(str.ToLower()) |> not)
    