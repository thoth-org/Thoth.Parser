module Thoth.Parser.Tests.Base.Numbers

open Fable.Pyxpecto
open Thoth.Parser.Base
open Thoth.Parser.Base.Numbers
open Thoth.Parser.Tests.Prelude
open System.Text

module Errors =

    let invalidSign = "Invalid sign"
    let invalidNumber = "Invalid number"

let private int8_tests =
    testList "int8"
    <| [
        test "int8 works with '127'" {
            let result = Parser.run (Parser.int8 Errors.invalidSign Errors.invalidNumber) "127"
            Assert.equal (result, Ok System.SByte.MaxValue)
        }

        test "int8 works with '-128'" {
            let result = Parser.run (Parser.int8 Errors.invalidSign Errors.invalidNumber) "-128"
            Assert.equal (result, Ok System.SByte.MinValue)
        }

        test "int8 fails with '128'" {
            let result = Parser.run (Parser.int8 Errors.invalidSign Errors.invalidNumber) "128"

            Assert.equal (
                result,
                {
                    Row = 1
                    Column = 4
                    Problem = Errors.invalidNumber
                    ContextStack = []
                }
                |> List.singleton
                |> Error
            )
        }

        test "int8 fails with '-129'" {
            let result = Parser.run (Parser.int8 Errors.invalidSign Errors.invalidNumber) "-129"

            Assert.equal (
                result,
                {
                    Row = 1
                    Column = 5
                    Problem = Errors.invalidNumber
                    ContextStack = []
                }
                |> List.singleton
                |> Error
            )
        }

        test "int8 fails on non digit" {
            let result = Parser.run (Parser.int8 Errors.invalidSign Errors.invalidNumber) "a"

            Assert.equal (
                result,
                {
                    Row = 1
                    Column = 1
                    Problem = Errors.invalidNumber
                    ContextStack = []
                }
                |> List.singleton
                |> Error
            )
        }

        test "int8 fails on empty string" {
            let result = Parser.run (Parser.int8 Errors.invalidSign Errors.invalidNumber) ""

            Assert.equal (
                result,
                {
                    Row = 1
                    Column = 1
                    Problem = Errors.invalidNumber
                    ContextStack = []
                }
                |> List.singleton
                |> Error
            )
        }

        test "int8 fails on '-' followed by an invalid number" {
            let result = Parser.run (Parser.int8 Errors.invalidSign Errors.invalidNumber) "-a"

            Assert.equal (
                result,
                {
                    Row = 1
                    Column = 2
                    Problem = Errors.invalidNumber
                    ContextStack = []
                }
                |> List.singleton
                |> Error
            )
        }
    ]

let private int16_tests =
    testList "int16"
    <| [
        test "int16 works with '32767'" {
            let result =
                Parser.run (Parser.int16 Errors.invalidSign Errors.invalidNumber) "32767"

            Assert.equal (result, Ok System.Int16.MaxValue)
        }

        test "int16 works with '-32768'" {
            let result =
                Parser.run (Parser.int16 Errors.invalidSign Errors.invalidNumber) "-32768"

            Assert.equal (result, Ok System.Int16.MinValue)
        }

        test "int16 fails with '32768'" {
            let result =
                Parser.run (Parser.int16 Errors.invalidSign Errors.invalidNumber) "32768"

            Assert.equal (
                result,
                {
                    Row = 1
                    Column = 6
                    Problem = Errors.invalidNumber
                    ContextStack = []
                }
                |> List.singleton
                |> Error
            )
        }

        test "int16 fails with '-32769'" {
            let result =
                Parser.run (Parser.int16 Errors.invalidSign Errors.invalidNumber) "-32769"

            Assert.equal (
                result,
                {
                    Row = 1
                    Column = 7
                    Problem = Errors.invalidNumber
                    ContextStack = []
                }
                |> List.singleton
                |> Error
            )
        }

        test "int16 fails on non digit" {
            let result = Parser.run (Parser.int16 Errors.invalidSign Errors.invalidNumber) "a"

            Assert.equal (
                result,
                {
                    Row = 1
                    Column = 1
                    Problem = Errors.invalidNumber
                    ContextStack = []
                }
                |> List.singleton
                |> Error
            )
        }

        test "int16 fails on empty string" {
            let result = Parser.run (Parser.int16 Errors.invalidSign Errors.invalidNumber) ""

            Assert.equal (
                result,
                {
                    Row = 1
                    Column = 1
                    Problem = Errors.invalidNumber
                    ContextStack = []
                }
                |> List.singleton
                |> Error
            )
        }

        test "int16 fails on '-' followed by an invalid number" {
            let result = Parser.run (Parser.int16 Errors.invalidSign Errors.invalidNumber) "-a"

            Assert.equal (
                result,
                {
                    Row = 1
                    Column = 2
                    Problem = Errors.invalidNumber
                    ContextStack = []
                }
                |> List.singleton
                |> Error
            )
        }
    ]

let private int32_tests =
    testList "int32"
    <| [
        test "int32 works with '2147483647'" {
            let result =
                Parser.run (Parser.int32 Errors.invalidSign Errors.invalidNumber) "2147483647"

            Assert.equal (result, Ok System.Int32.MaxValue)
        }

        test "int32 works with '-2147483648'" {
            let result =
                Parser.run (Parser.int32 Errors.invalidSign Errors.invalidNumber) "-2147483648"

            Assert.equal (result, Ok System.Int32.MinValue)
        }

        test "int32 fails with '2147483648'" {
            let result =
                Parser.run (Parser.int32 Errors.invalidSign Errors.invalidNumber) "2147483648"

            Assert.equal (
                result,
                {
                    Row = 1
                    Column = 11
                    Problem = Errors.invalidNumber
                    ContextStack = []
                }
                |> List.singleton
                |> Error
            )
        }

        test "int32 fails with '-2147483649'" {
            let result =
                Parser.run (Parser.int32 Errors.invalidSign Errors.invalidNumber) "-2147483649"

            Assert.equal (
                result,
                {
                    Row = 1
                    Column = 12
                    Problem = Errors.invalidNumber
                    ContextStack = []
                }
                |> List.singleton
                |> Error
            )
        }

        test "int32 fails on non digit" {
            let result = Parser.run (Parser.int32 Errors.invalidSign Errors.invalidNumber) "a"

            Assert.equal (
                result,
                {
                    Row = 1
                    Column = 1
                    Problem = Errors.invalidNumber
                    ContextStack = []
                }
                |> List.singleton
                |> Error
            )
        }

        test "int32 fails on empty string" {
            let result = Parser.run (Parser.int32 Errors.invalidSign Errors.invalidNumber) ""

            Assert.equal (
                result,
                {
                    Row = 1
                    Column = 1
                    Problem = Errors.invalidNumber
                    ContextStack = []
                }
                |> List.singleton
                |> Error
            )
        }

        test "int32 fails on '-' followed by an invalid number" {
            let result = Parser.run (Parser.int32 Errors.invalidSign Errors.invalidNumber) "-a"

            Assert.equal (
                result,
                {
                    Row = 1
                    Column = 2
                    Problem = Errors.invalidNumber
                    ContextStack = []
                }
                |> List.singleton
                |> Error
            )
        }
    ]

let tests =
    testList
        "Numbers"
        [
            int8_tests
            int16_tests
        ]
