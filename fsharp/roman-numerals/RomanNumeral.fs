module RomanNumeral
open System.Text

let numVals = [(1000,"M"); (900,"CM"); (500,"D");
                (400,"CD"); (100,"C"); (90,"XC");
                (50,"L"); (40,"XL"); (10,"X");
                (9,"IX"); (5,"V"); (4,"IV");
                (1,"I")]  

let toRoman n = 
    let rec accfun (acc : StringBuilder) num =
        let numFun n = List.tryFind (fun x -> n / (fst x) > 0) numVals
        match numFun num with
        | Some(x,y) -> accfun (acc.Append(y)) (num - x)
        | None -> acc.ToString()
    accfun (new StringBuilder()) n