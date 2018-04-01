module Pangram
// didn't know you could do this
let alphabet = ['a' .. 'z'] |> set

let isPangram (str : string) =
    str.ToLowerInvariant()
    |> Set.ofSeq
    |> Set.isSubset alphabet