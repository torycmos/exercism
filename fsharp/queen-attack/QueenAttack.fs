module QueenAttack

let canAttack (x : int * int) (y : int * int) =
    if x = y then invalidOp "Can't occupy same space"
    // same col/row/diag; had them in some elifs but that was just if true then true 
    // which seems p silly
    else fst x = fst y || snd x = snd y || (abs(fst x - snd x) = abs(fst y - snd y)) 