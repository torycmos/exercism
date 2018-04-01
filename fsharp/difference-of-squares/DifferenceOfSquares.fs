module DifferenceOfSquares

let square x = x * x
let sums f n = [1..n] |> f

let squareOfSums = sums List.sum >> square
let sumOfSquares = sums (List.map square >> List.sum)

let difference x = squareOfSums x - sumOfSquares x


