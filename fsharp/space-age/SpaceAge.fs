module SpaceAge

open System

type Planet = | Earth | Mercury | Venus | Mars | Jupiter | Saturn | Uranus | Neptune

let convert = 
    Map.ofList [  Mercury, 0.2408467m;
                  Venus, 0.61519726m;
                  Mars, 1.8808158m;
                  Jupiter, 11.862615m;
                  Saturn, 29.447498m;
                  Uranus, 84.016846m;
                  Neptune, 164.79132m  ]

let earthTime (secs : decimal) = System.TimeSpan.FromSeconds(float(secs)).TotalDays / 365.25 |> decimal

let round = fun (x : Decimal) -> Math.Round(x,2)

let spaceAge (planet : Planet) (secs : decimal) = 
    match planet with
    | Earth -> earthTime secs |> round
    | x -> earthTime secs / convert.Item(x) |> round