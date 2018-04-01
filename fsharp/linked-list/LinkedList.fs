module Deque
open System

//spent a bunch of ultimately fruitless time trying to figure a way to
//make this work immutably; eventually figured out that with how the 
//tests use/bind the deque it's not gonna happen. Well, or I just never
//figured out how.
type DilzList<'a> =
    {   mutable L : 'a list;
        mutable R : 'a list;  }

let checkEnd (a : 'a list) (b : 'a list) =
    match a,b with
    | [], r -> 
        if r.Length > 1 then
            let x,y = List.splitAt (r.Length / 2) (r |> List.rev)
            x, (y |> List.rev)
        else r,[]
    | l, [] -> 
        if l.Length > 1 then
            let x,y = List.splitAt (l.Length / 2) l
            x, (y |> List.rev) 
        else [],l
    | _ -> a,b

type Deque<'a>() = 
    let stacks = {L = []; R = []}
    let set (x : 'a list) (y : 'a list) = 
        stacks.L <- x
        stacks.R <- y

    member t.push x =
        stacks.L <- x::stacks.L
    member t.unshift x =
        stacks.R <- x::stacks.R

    member t.pop () =
        match stacks.L, stacks.R with
        | x::xs,y ->
            set <|| (xs, y)
            x
        | [], y::ys as z ->
            z ||> checkEnd ||> set
            t.pop()
        | [],[] -> invalidOp "empty"

    member t.shift () =
        match stacks.L, stacks.R with
        | x,y::ys ->
            set <|| (x, ys)
            y
        | x::xs, [] as z-> 
            z ||> checkEnd ||> set
            t.shift()
        | [],[] -> invalidOp "empty"
