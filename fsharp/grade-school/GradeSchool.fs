module School

let empty = []

let add name grade (school : (string * int) list) =
    (name, grade) :: school

let grade n school = 
    List.filter (fun x -> snd x = n) school
    |> List.map fst
    |> List.sort

let roster (school : (string * int) list) = 
    List.distinctBy snd school
    |> List.fold (fun acc (name, grd) ->
                    (grd,(grade grd school)) :: acc) []
    |> List.sort