
// this one uses a sequence; the generator returns all multiples of a given number
// up to the max value. sumOfMultiples takes all the sequences produced by the input list
// and makes them a set to filter dupe values and then sums em.
let multiples upperExclusive number =
    Seq.initInfinite (fun i -> (i + 1) * number)
    |> Seq.takeWhile (fun x -> x < upperExclusive)

let sumOfMultiples numbers upperExclusive =
    numbers
    |> Seq.collect (multiples upperExclusive)
    |> set
    |> Seq.sum