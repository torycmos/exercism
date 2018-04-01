module Trinary
open System.Text.RegularExpressions
let toInt (x : char) = (int x) - (int '0')

let toRadix r (str : string) =
    str.ToCharArray()
    |> Array.rev
    |> Array.mapi (fun n x -> (toInt x) * (pown r n))
    |> Array.sum 

let toDecimal (str : string)=
    if Regex.Match(str,"[^012]").Success then 0
    else toRadix 3 str