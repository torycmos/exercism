module WordCount

open System
open System.Text.RegularExpressions

let regx = Regex(@"(\w+'?\w*)\W*")

// lookin way ugly but I'm so done with messing with regexes atm so f it
let wordCount (str : string) =
    str.ToLower().Split([|' ';',';';';':';'.'|])
    |> Seq.map (fun x -> x.Trim([|'''|]))
    |> Seq.map (fun x -> regx.Replace(x,"$1"))
    |> Seq.filter (String.IsNullOrEmpty >> not)
    |> Seq.groupBy(id)
    |> Map.ofSeq
    |> Map.map (fun k v -> Seq.length v)
