module KinderGartenGarden
open System

type Plant = Radishes | Clover | Grass | Violets

let plants = 
    [   ('V',Violets)
        ('R',Radishes)
        ('C',Clover)
        ('G',Grass)
    ] |> Map.ofSeq

let defaultKids = ["Alice"; "Bob"; "Charlie"; "David";
                    "Eve"; "Fred"; "Ginny"; "Harriet";
                    "Ileana"; "Joseph"; "Kincaid";"Larry"]

let lookupPlants (kid : string) (garden : Map<string,int> * seq<char list>) = 
    match Map.tryFind kid (fst garden) with
    |Some x -> Seq.item x (snd garden)
            |> List.map (fun x -> Map.find x plants)
    |None -> []
    
let garden (kids : string list) (str : string) = 
    let kidmap = 
        kids
        |> List.sort
        |> List.indexed
        |> List.map (fun (x,y) -> y,x)
        |> Map.ofList

    let gardenstr =
        str.ToUpperInvariant().Split('\n')
        |> (fun x -> x.[0], x.[1])
        ||> (fun x y -> seq { 
                for i in [0 .. 2 .. (str.Length / 2 - 1)] do
                    yield [x.[i];x.[i+1];y.[i];y.[i+1]]
                })
    kidmap, gardenstr

let defaultGarden (str : string) = 
    garden defaultKids str