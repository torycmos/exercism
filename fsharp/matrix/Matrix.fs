module Matrix
open System

let fromString (str : string) = 
    str.Split('\n')
    |> Array.map (fun (x : string) -> x.Split(' '))
    |> (fun x -> Array.map (Array.map Int32.Parse) x)

let rows = id
let cols (matrix : int [][]) = 
    [| for i in 0 .. (Array.head matrix).Length - 1 do
       yield Array.map (Array.item i) matrix
    |]
    