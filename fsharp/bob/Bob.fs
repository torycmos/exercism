module Bob

open System
open System.Text.RegularExpressions

let (|Sure|Chill|Fine|Whatevs|) (str : string) =
    if 
        str.Trim() = "" then Fine
    elif
        (str.ToUpper() = str) && Regex.IsMatch(str, "[a-zA-Z]") then Chill
    elif
        str.EndsWith("?") then Sure
    else 
        Whatevs

let hey = function
    | Sure -> sprintf "Sure."
    | Chill -> sprintf "Whoa, chill out!"
    | Fine -> sprintf "Fine. Be that way!"
    | Whatevs -> sprintf "Whatever."
