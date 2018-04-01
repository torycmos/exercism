module PigLatin

open System
open System.Text.RegularExpressions

let consPats = ["qu";".qu";"ch";"thr";"th";"sch"]
let vowPats = ["yt";"xr";"a";"e";"i";"o";"u"]

let regexr x y= 
    Regex.Match(x,"^" + y).Value

let (|Vowel|Cons|) (str : string) =
    let regMatch patternList =
        (List.map (regexr str) patternList)
        // this map produces empty strings and possibly multiple matches, 
        // so choose the best one
        |> List.maxBy (String.length)

    if not(String.IsNullOrEmpty(regMatch vowPats)) 
    then Vowel
    else Cons(regMatch consPats)
    

let translate (str : string) =
    str.Split([|' '|])
    |> Array.map (fun x ->
        match x with
        | Vowel         ->  str + "ay"
        | Cons (str)    ->  if String.IsNullOrEmpty(str) then
                                x.Substring(1) + x.Substring(0,1) + "ay"
                            else x.Substring(str.Length) + str + "ay"
        )
    |> String.concat(" ")

