module BankAccount

type BankAccount() = 
    
    let mutable (balance : float option) = None
    member t.Balance = balance

    member t.openAccount() = 
        balance <- Some(0.0)

    member t.updateBalance(x : float) = 
        if balance.IsSome then balance <- Some(balance.Value + x)
        else failwith("account closed")

    member t.getBalance() = balance.Value |> decimal

    member t.closeAccount() = 
        balance <- None