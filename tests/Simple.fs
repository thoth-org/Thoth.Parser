module Thoth.Parser.Tests.Simple

open Fable.Pyxpecto
open Thoth.Parser
open Thoth.Parser.Operators

type Point =
    {
        X: int
        Y: int
    }

module Parser =

    let boolean =
        Parser.oneOf
            [
                Parser.succeed true |. (Parser.keyword "true")

                Parser.succeed false |. (Parser.keyword "false")
            ]

    let point =
        Parser.succeed (fun x y ->
            {
                X = x
                Y = y
            }
        )
        |. Parser.token "("
        |. Parser.spaces
        |= Parser.int32
        |. Parser.spaces
        |. Parser.token ","
        |. Parser.spaces
        |= Parser.int32
        |. Parser.spaces
        |. Parser.token ")"
        |. Parser.exhausted

let tests =
    testList
        "Parser.Simple"
        [
            test "Parser.boolean works with 'false'" {
                let result = Parser.run Parser.boolean "false"

                Assert.equal (result, Ok false)
            }

            test "Parser.boolean works with 'true'" {
                let result = Parser.run Parser.boolean "true"

                Assert.equal (result, Ok true)
            }

            test "Parser.boolean fails with 'tru'" {
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
            }

            testList
                "Point"
                [
                    test "Parser.point works with '(1, 2)'" {
                        let result = Parser.run Parser.point "(1, 2)"

                        Assert.equal (
                            result,
                            Ok
                                {
                                    X = 1
                                    Y = 2
                                }
                        )
                    }

                    test "Parser.point fails with '(1, 2'" {
                        let result = Parser.run Parser.point "(1, 2"

                        Assert.equal (
                            result,
                            Error
                                [
                                    {
                                        Row = 1
                                        Column = 6
                                        Problem = Problem.Expecting ")"
                                    }
                                ]
                        )
                    }

                    test "Parser.point fails if not all the characters are consumed" {
                        let result = Parser.run Parser.point "(1, 2)xxx"

                        Assert.equal (
                            result,
                            Error
                                [
                                    {
                                        Row = 1
                                        Column = 7
                                        Problem = Problem.ExpectingEnd
                                    }
                                ]
                        )
                    }

                    test "Parser.point fails if the first number is missing" {
                        let result = Parser.run Parser.point "(, 2)"

                        Assert.equal (
                            result,
                            Error
                                [
                                    {
                                        Row = 1
                                        Column = 2
                                        Problem = Problem.ExpectingInt32
                                    }
                                ]
                        )
                    }

                    test "Parser.point fails if the comma is missing" {
                        let result = Parser.run Parser.point "(1 2)"

                        Assert.equal (
                            result,
                            Error
                                [
                                    {
                                        Row = 1
                                        Column = 4
                                        Problem = Problem.Expecting ","
                                    }
                                ]
                        )
                    }

                    test "Parser.point fails if the second number is missing" {
                        let result = Parser.run Parser.point "(1,)"

                        Assert.equal (
                            result,
                            Error
                                [
                                    {
                                        Row = 1
                                        Column = 4
                                        Problem = Problem.ExpectingInt32
                                    }
                                ]
                        )
                    }

                    test "Parser.point works with spaces" {
                        let result = Parser.run Parser.point "(  1    ,   2    )"

                        Assert.equal (
                            result,
                            Ok
                                {
                                    X = 1
                                    Y = 2
                                }
                        )
                    }
                ]
        ]
