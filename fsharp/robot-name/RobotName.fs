module RobotName

open System
open System.Text

type Robot(iv : int) =    
    
     // static serial number field makes sure each new robot / 
     // name change uses a new seed value
    static let mutable serialNum = 0    
    
    let newName (x : int) =
        let rand = Random(x)
        let name = StringBuilder()
        let letters n =
            [| for i in [1..n] do
                   yield char (rand.Next(0,25)) + 'A'
            |]
        let number = rand.Next(100,999)

        name.Append(letters 2).Append(number).ToString()


    let mutable name = newName serialNum 
    
    member t.Name = name

    member t.Reset() = 
        serialNum <- serialNum + 1
        name <- newName serialNum

    new() =
        serialNum <- serialNum + 1 
        new Robot(serialNum)

