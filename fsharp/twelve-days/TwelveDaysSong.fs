module TwelveDaysSong
open System.Text

let append (text: string) (builder : StringBuilder) =
    builder.Append(text)
let string (builder : StringBuilder) = builder.ToString()

let giftMap = [ "zeroth", "" // just to 1-index this
                "first", "a Partridge in a Pear Tree"
                "second", "two Turtle Doves"
                "third", "three French Hens"
                "fourth", "four Calling Birds"
                "fifth", "five Gold Rings"
                "sixth", "six Geese-a-Laying"
                "seventh", "seven Swans-a-Swimming"
                "eighth", "eight Maids-a-Milking"
                "ninth", "nine Ladies Dancing"
                "tenth", "ten Lords-a-Leaping"
                "eleventh", "eleven Pipers Piping"
                "twelfth", "twelve Drummers Drumming" ] 

let beginDay i = 
    new StringBuilder()
    |> append ("On the " + (fst (List.item i giftMap)) 
                         + " day of Christmas my true love gave to me, ")
let lastDay = append ("and a Partridge in a Pear Tree.\n")

let verse day = 
    let folder acc day = 
        acc
        |> append (snd (List.item day giftMap))
        |> append ", "
    if day = 1 then 
        beginDay 1 
        |> lastDay
        |> (fun x -> x.Replace("and ",""))
    else
        [day .. -1 .. 2]
        |> List.fold folder (beginDay day)
        |> lastDay
    |> string

let verses n m = 
    [n .. m]
    |> List.map verse
    |> String.concat("\n")
    |> (fun x -> x + "\n")

let song = verses 1 12