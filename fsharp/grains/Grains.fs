module Grains

open System.Numerics

let square x = 1uL <<< (x - 1) |> BigInteger

let total = (square 64 * 2I) - 1I