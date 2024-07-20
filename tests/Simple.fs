module Thoth.Parser.Tests.Simple

open Fable.Pyxpecto
open Thoth.Parser
open Thoth.Parser.Operators

module Parser =

    let boolean =
        Parser.oneOf
            [
                Parser.succeed true |. (Parser.keyword "true")

                Parser.succeed false |. (Parser.keyword "false")
            ]

let tests =
    testList
        "Parser.Simple"
        [
            testCase "Parser.boolean works with 'false'"
            <| fun _ ->
                let result = Parser.run Parser.boolean "false"

                Assert.equal (result, Ok false)

            testCase "Parser.boolean works with 'true'"
            <| fun _ ->
                let result = Parser.run Parser.boolean "true"

                Assert.equal (result, Ok true)

            testCase "Parser.boolean fails with 'tru'"
            <| fun _ ->
                let result = Parser.run Parser.boolean "tru"

                Assert.equal (
                    result,
                    Error
                        [
                            {
                                Row = 1
                                Column = 1
                                Problem = Problem.Expecting "true"
                            }
                            {
                                Row = 1
                                Column = 1
                                Problem = Problem.Expecting "false"
                            }
                        ]
                )

            test "Version" {
                let pDot = Parser.token "."

                let parser =
                    Parser.succeed (fun (major: int32) (minor: int32) (patch: int32) ->
                        (major, minor, patch)
                    )
                    |> Parser.keep Parser.int32
                    |> Parser.drop pDot
                    |> Parser.keep Parser.int32
                    |> Parser.drop pDot
                    |> Parser.keep Parser.int32

                let result = Parser.run parser "1T2.39999"

                // Assert.equal (result, Ok(1, 2, 3))
                match result with
                | Ok _ -> failwith "Expected an error"
                | Error deadEnds -> printfn "%A" (Parser.deadEndsToString deadEnds)

            }
        ]
