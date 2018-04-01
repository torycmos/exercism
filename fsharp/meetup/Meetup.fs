module Meetup
open System
open System.Linq

type Schedule = First | Second | Third | Fourth | Teenth | Last
let teenths = [13..19]
let daysInMonth year month = 
        [for d in 1 ..  DateTime.DaysInMonth(year, month) do
            yield DateTime(year, month, d)
        ]

let nthDay = function
    | First -> Seq.item 0
    | Second -> Seq.item 1
    | Third -> Seq.item 2
    | Fourth -> Seq.item 3
    | Last -> Seq.last
    | Teenth -> Seq.find (fun (x : DateTime) -> x.Day > 12)

let meetupDay (day : DayOfWeek) (sched : Schedule) year month =
    let days = daysInMonth year month    
    query {
        for d in days do
        where (d.DayOfWeek = day)
        select d.Date
        } |> (nthDay sched)
