module Clock

type Clock(hours : int, mins : int) =
    let time =  ((60 * hours) + mins) % (24 * 60)

    let timeFunc t = 
        let hours = t / 60
        let mins = t % 60
        sprintf "%02d:%02d" hours mins

    override t.ToString() = timeFunc time

    member t.add n = new Clock(hours, mins + n)
    member t.subtract n = 
        // if the clock would underflow, try it with n reduced by 24 hours
        // (an arbitrary number of times with %)
        if time - n < 0 then new Clock(hours,1440 - (n % 1440) + 1)
        else new Clock(hours,mins - n)

    override t.Equals(clock) = 
        match clock with
        | :? Clock as x -> t.ToString() = x.ToString()
        | _ -> false

    new (hours : int) =
        new Clock(hours, 0)