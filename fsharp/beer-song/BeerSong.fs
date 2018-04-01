module BeerSong

open System.Text

let botString1 = function
    | 0 -> "no more bottles of beer"
    | 1 -> "1 bottle of beer"
    | x -> x.ToString() + " bottles of beer"

let botString2 = function
    | 0 -> "No more bottles of beer on the wall"
    | x -> botString1 x + " on the wall"

let botString3 = function
    | 0 -> "Go to the store and buy some more, "
    | 1 -> "Take it down and pass it around, "
    | _ -> "Take one down and pass it around, "

let append (text: string) (builder : StringBuilder) =
    builder.Append(text)

let verse x =
    let foo = StringBuilder()
    foo.Append(botString2 x + ", " + botString1 x + ".\n")
       .Append(botString3 x + (botString2 (if x = 0 then 99
                                           else x - 1)).ToLower() + ".\n")
       .ToString()

let verses x y =
    [x .. -1 .. y]
    |> (List.fold (fun (acc : StringBuilder) x -> acc.Append(verse x).Append("\n")) 
                                                 (StringBuilder()))
    |> (fun (x : StringBuilder) -> x.ToString())

let sing = verses 99 0

// thing's kind of a beast but I've had enough for now
