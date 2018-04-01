module Raindrops
open System.Text

let divisors = [(3,"Pling")
                (5,"Plang")
                (7,"Plong")]  
let divisible n m = n % m = 0

let convert n =
    List.fold (fun (acc : StringBuilder) 
                   (x : int * string) ->
                        if (fst x |> divisible n) 
                        then acc.Append(snd x)
                        else acc)
              (new StringBuilder()) divisors
    |> (fun x -> if x.Length = 0 
                 then x.Append(n).ToString()
                 else x.ToString())