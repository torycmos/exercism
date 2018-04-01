module Atbash

open System.Text

let digits = ['0'..'9']
let alphabet = ['a'..'z']
let cipher = 
    List.zip digits digits
    |> List.append (List.zip alphabet (List.rev alphabet))
    |> Map.ofList 

let encode (input : string) =
    input.ToLower().ToCharArray()
    |> Array.filter (fun x -> Map.containsKey x cipher)
    |> Array.map (fun x -> cipher.Item x)  //feel like there's something to combine these
    |> Array.fold (fun (acc : StringBuilder) x -> 
                        if acc.Length % 6 = 0 then acc.Append([| ' '; x |])
                        else acc.Append(x))
                   (StringBuilder())
    |> (fun x -> x.ToString().Trim())