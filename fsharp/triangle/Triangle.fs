module Triangle

open System

type TriangleKind = Equilateral|Isosceles|Scalene

let kind (x : decimal) (y : decimal)  (z : decimal) =
    let legs = List.sortDescending [x;y;z]
    // if the longest leg is longer than the other two combined then it's an invalid triangle;
    // this is also always going to be the case for < 0 length legs so I got rid of that predicate
    if  
        legs.Head >= (List.reduce (+) legs.Tail) then 
            raise (InvalidOperationException("Violates triangle inequality"))
    else 
        match Set(legs).Count with
        | 1 -> Equilateral
        | 2 -> Isosceles
        | 3 -> Scalene
        | _ -> raise (InvalidOperationException())