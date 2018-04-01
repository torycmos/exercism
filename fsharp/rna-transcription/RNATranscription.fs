module RNATranscription

let mapping = [('G','C');('C','G');('T','A');('A','U')] |> Map.ofList

let toRna = String.map (fun x -> Map.find x mapping) 