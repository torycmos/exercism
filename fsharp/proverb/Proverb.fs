module Proverb

let getThing (n : int) = 
    let things = ["nail";"shoe";"horse";"rider";"message";"battle";"kingdom"]
    List.pairwise things
    |> List.item (n - 1)
    

let line n = 
    match n with
    | _ when n < 7 -> 
        sprintf "For want of a %s the %s was lost." <|| getThing n
    | _ -> sprintf "And all for the want of a horseshoe nail."

let proverb =
    [for i in 1..7 -> line i]
    |> String.concat "\n"
