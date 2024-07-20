module Thoth.Parser.Tests.Main

open Fable.Pyxpecto

[<EntryPoint>]
let main argv =
    testList
        "All"
        [
            LowLevel.tests
            Base.Common.tests
            Base.Numbers.tests
            Simple.tests
        ]
    |> Pyxpecto.runTests [||]
