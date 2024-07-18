module Thoth.Parser.Tests.Main

open Fable.Pyxpecto

[<EntryPoint>]
let main argv =
    testList "All" [ Parser.LowLevel.all; Parser.Simple.tests ]
    |> Pyxpecto.runTests [||]
