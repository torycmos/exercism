module SumOfMultiples

let isMultiple (xs : int list) (i : int) =
    xs
    |> List.exists (fun x -> i % x = 0)

let sumOfMultiples (xs : int list) (max : int) =
    [1..max-1]
    |> List.filter (isMultiple xs)
    |> List.sum

