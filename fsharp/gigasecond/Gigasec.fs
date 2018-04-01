module Gigasecond
open System

//simplified a bit
let gigasecond (date: DateTime) =
    date.AddSeconds(1e9).Date