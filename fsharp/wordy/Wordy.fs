module WordProblem
open System

type Token = Add | Sub | Mult | Div | Num of int | EOF | WhatIs | InvalidOp
let opStrings = [ ("plus",Add)
                  ("minus", Sub)
                  ("multiplied",Mult)
                  ("divided",Div) ] |> Map.ofList

let listify (str : string) = 
    str.TrimEnd([|'?'|]).Split(' ')
    |> List.ofArray
    
let (|CheckStart|_|) str = 
    match listify str with
    | "What"::"is"::xs -> Some(WhatIs, xs)
    | _ -> None

let (|CheckNum|_|) = function
    | x::xs' ->
        let res,n = Int32.TryParse(x)
        if res then Some(Num(n), xs')
        else  None
    | [] -> Some(EOF,[])

let (|CheckOp|_|) = function
    | x::xs ->
        let op = Map.tryFind x opStrings
        if op.IsNone then None
        else
            match op.Value with
            | Add | Sub -> Some (op.Value,xs)
            | Div | Mult ->
                match xs with
                | "by"::xs' -> Some(op.Value,xs')
                | _ -> None
            | _ -> None
    | [] -> Some(EOF,[])

let tokenize str =
    let rec loop acc (xs : string list) =
        match xs with
        | CheckNum (EOF, _) -> EOF::acc
        | CheckNum (num, xs') -> loop (num::acc) xs'
        | CheckOp (EOF, _) -> EOF::acc
        | CheckOp (op, xs') -> loop (op::acc) xs'
        | _ -> printfn "invalid token: %s" (List.head xs) ; [InvalidOp]
    match str with
    | CheckStart(WhatIs, xs) -> loop [] xs
    | _ -> [InvalidOp]

let parse tokens =
    List.foldBack (fun x acc ->
        match x, acc with
        | EOF, _ -> acc
        | (Add | Sub | Mult | Div), _ -> x::acc
        | Num(n), op::num::acc' -> 
            match op, num with
            | Add, Num(x) -> Num(x + n) :: acc'
            | Sub, Num(x) -> Num(x - n) :: acc'
            | Div, Num(x) -> Num(x / n) :: acc'
            | Mult, Num(x) -> Num(x * n) :: acc'
            | _ -> (printfn "can't operate on %O and %O" op num); [InvalidOp] 
        | _,_ -> x::acc
        ) tokens [] 
    |> (fun x -> match x with
                 | [Num x] -> Some x
                 | _ -> None)

let solve = tokenize >> parse