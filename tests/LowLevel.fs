module Thoth.Parser.Tests.LowLevel

open Fable.Pyxpecto
open Thoth.Parser.LowLevel
open System.Text

let tests =
    testList
        "LowLevel"
        [
            testList
                "FindSubString"
                [

                    testCase
                        "works when the searched string is at the beginning"
                        (fun () ->
                            let actual = findSubString "42" 0 1 1 "42 is the answer!"

                            Assert.equal (
                                actual,
                                CursorPosition.Create 2 1 3 |> SubStringResult.Match
                            )
                        )

                    testCase
                        "works when the searched string is at the end"
                        (fun () ->
                            let actual = findSubString "answer!" 0 1 1 "42 is the answer!"

                            Assert.equal (
                                actual,
                                CursorPosition.Create 17 1 18 |> SubStringResult.Match
                            )
                        )

                    testCase
                        "works when the searched string is in the middle"
                        (fun () ->
                            let actual = findSubString "is" 0 1 1 "42 is the answer!"

                            Assert.equal (
                                actual,
                                CursorPosition.Create 5 1 6 |> SubStringResult.Match
                            )
                        )

                    testCase
                        "works when the searched string is at the beginning of a new line"
                        (fun () ->
                            let actual = findSubString "42" 0 1 1 "Is \n\n\n42\nthe answer?"

                            Assert.equal (
                                actual,
                                CursorPosition.Create 8 4 3 |> SubStringResult.Match
                            )
                        )

                    testCase
                        "make sure the same column is returned relativy to row position"
                        (fun () ->
                            let actual = findSubString "42" 0 1 1 "Is 42 the answer?"

                            Assert.equal (
                                actual,
                                CursorPosition.Create 5 1 6 |> SubStringResult.Match
                            )

                            let actual = findSubString "42" 0 1 1 "\nIs 42 the answer?"

                            Assert.equal (
                                actual,
                                CursorPosition.Create 6 2 6 |> SubStringResult.Match
                            )
                        )

                    testCase
                        "works with unicode taking 2 bytes"
                        (fun () ->
                            let actual = findSubString "👍" 0 1 1 "Great 👍"

                            Assert.equal (
                                actual,
                                CursorPosition.Create 8 1 8 |> SubStringResult.Match
                            )

                            let actual = findSubString "👍" 0 1 1 "This is a 👍 great emoji"

                            Assert.equal (
                                actual,
                                CursorPosition.Create 12 1 12 |> SubStringResult.Match
                            )

                            let actual = findSubString "👍" 0 1 1 "🚀 This is a 👍 great emoji"

                            Assert.equal (
                                actual,
                                CursorPosition.Create 15 1 14 |> SubStringResult.Match
                            )
                        )

                    testCase
                        "returns NoMatch if the searched string is not found"
                        (fun () ->
                            let actual = findSubString "42" 0 1 1 "Is the answer?"

                            Assert.equal (actual, Position.Create 1 15 |> SubStringResult.NoMatch)
                        )
                ]

            testList
                "isAsciiCode"
                [

                    testCase
                        "works with ascii code"
                        (fun () ->
                            let actual = isAsciiCode 97 4 "xxxxa"

                            Assert.equal (actual, true)

                            let actual = isAsciiCode 97 0 "a"

                            Assert.equal (actual, true)

                            let actual = isAsciiCode 125 4 "xxxx}"

                            Assert.equal (actual, true)
                        )

                    testCase
                        "returns false if the character is not an ascii code"
                        (fun () ->
                            let actual = isAsciiCode 97 4 "xxxx👍"

                            Assert.equal (actual, false)
                        )

                    testCase
                        "returns false if the character is different from the ascii code"
                        (fun () ->
                            let actual = isAsciiCode 97 4 "xxxxb"

                            Assert.equal (actual, false)
                        )

                ]

            testList
                "charMatchAt"
                [

                    testCase
                        "returns NoMatch if offset is out of bounds"
                        (fun () ->
                            let actual = charMatchAt (fun _ -> true) 10 "abc"

                            Assert.equal (actual, CharMatchAtResult.NoMatch)
                        )

                    testCase
                        "returns 'offset + 1' if the character matches and is encoded on 1 bytes in UTF-16"
                        (fun () ->
                            let actual = charMatchAt (fun rune -> rune = Rune 'a') 0 "abc"

                            Assert.equal (actual, CharMatchAtResult.Match 1)
                        )

                    testCase
                        "returns 'offset + 2' if the character matches and is encoded on 2 bytes in UTF-16"
                        (fun () ->
                            let actual =
                                charMatchAt (fun rune -> rune = Rune.GetRuneAt("👍", 0)) 0 "👍abc"

                            Assert.equal (actual, CharMatchAtResult.Match 2)
                        )
                ]

            testList
                "isSubStringAt"
                [
                    testCase
                        "works when the searched string is at the beginning"
                        (fun () ->
                            let actual = isSubStringAt "let" 0 1 1 "let x = 42"

                            Assert.equal (
                                actual,
                                CursorPosition.Create 3 1 4 |> IsSubStringAtResult.Match
                            )
                        )

                    testCase
                        "works when the searched string is at the end"
                        (fun () ->
                            let actual = isSubStringAt "42" 8 1 1 "let x = 42"

                            Assert.equal (
                                actual,
                                CursorPosition.Create 10 1 3 |> IsSubStringAtResult.Match
                            )
                        )

                    testCase
                        "works when searching in string with 2 bytes unicode character"
                        (fun () ->
                            let actual = isSubStringAt "Great 👍" 9 1 1 "let x = \"Great 👍 work\""

                            Assert.equal (
                                actual,
                                CursorPosition.Create 17 1 8 |> IsSubStringAtResult.Match
                            )
                        )

                    testCase
                        "works for search for a 2 bytes unicode character"
                        (fun () ->
                            let actual = isSubStringAt "👍" 15 1 1 "let x = \"Great 👍 work\""

                            Assert.equal (
                                actual,
                                CursorPosition.Create 17 1 2 |> IsSubStringAtResult.Match
                            )
                        )

                    testCase
                        "works for search for a 2 bytes unicode character at the end"
                        (fun () ->
                            let actual = isSubStringAt "👍" 8 1 1 "let x = 👍"

                            Assert.equal (
                                actual,
                                CursorPosition.Create 10 1 2 |> IsSubStringAtResult.Match
                            )
                        )

                    testCase
                        "returns no match if the searched string is not found at the given offset"
                        (fun () ->
                            let actual = isSubStringAt "42" 0 1 1 "let x = 42"

                            Assert.equal (actual, IsSubStringAtResult.NoMatch)
                        )

                    testCase
                        "returns NoMatch is the searched string is not found (overflows at the end)"
                        (fun () ->
                            let actual = isSubStringAt "42;" 8 1 1 "let x = 42"

                            Assert.equal (actual, IsSubStringAtResult.NoMatch)
                        )

                ]

            testList
                "chompBase10"
                [

                    test "works with a single digit" {
                        let actual = chompBase10 0 "0"

                        Assert.equal (actual, 1)
                    }

                    test "supports all the digits" {
                        let actual = chompBase10 0 "0123456789"

                        Assert.equal (actual, 10)
                    }

                    test "stops at the first non digit character" {
                        let actual = chompBase10 0 "0123a456789"

                        Assert.equal (actual, 4)
                    }

                ]

            testList
                "consumeBase16"
                [

                    testCase
                        "works with a single digit"
                        (fun () ->
                            let actual = consumeBase16 0 "0"

                            Assert.equal (actual, (1, 0))
                        )

                    testCase
                        "supports all the base16 symbols"
                        (fun () ->
                            // Check all the symbols and their values
                            Assert.equal (consumeBase16 0 "0", (1, 0))
                            Assert.equal (consumeBase16 0 "1", (1, 1))
                            Assert.equal (consumeBase16 0 "2", (1, 2))
                            Assert.equal (consumeBase16 0 "3", (1, 3))
                            Assert.equal (consumeBase16 0 "4", (1, 4))
                            Assert.equal (consumeBase16 0 "5", (1, 5))
                            Assert.equal (consumeBase16 0 "6", (1, 6))
                            Assert.equal (consumeBase16 0 "7", (1, 7))
                            Assert.equal (consumeBase16 0 "8", (1, 8))
                            Assert.equal (consumeBase16 0 "9", (1, 9))
                            Assert.equal (consumeBase16 0 "a", (1, 10))
                            Assert.equal (consumeBase16 0 "b", (1, 11))
                            Assert.equal (consumeBase16 0 "c", (1, 12))
                            Assert.equal (consumeBase16 0 "d", (1, 13))
                            Assert.equal (consumeBase16 0 "e", (1, 14))
                            Assert.equal (consumeBase16 0 "f", (1, 15))
                            Assert.equal (consumeBase16 0 "A", (1, 10))
                            Assert.equal (consumeBase16 0 "B", (1, 11))
                            Assert.equal (consumeBase16 0 "C", (1, 12))
                            Assert.equal (consumeBase16 0 "D", (1, 13))
                            Assert.equal (consumeBase16 0 "E", (1, 14))
                            Assert.equal (consumeBase16 0 "F", (1, 15))
                            // Check that multiple symbols are supported
                            Assert.equal (consumeBase16 0 "A1F4", (4, 41460))
                        )

                    testCase
                        "stops at the first non base16 symbol"
                        (fun () ->
                            Assert.equal (consumeBase16 0 "A1F4Z4", (4, 41460))
                            Assert.equal (consumeBase16 0 "Z4A1F4", (0, 0))
                        )

                ]

            testList
                "consumeBase"
                [
                    testCase
                        "works with base 2"
                        (fun () ->
                            Assert.equal (consumeBase 2 0 "01", (2, 1))
                            Assert.equal (consumeBase 2 0 "0101", (4, 5))
                        )

                    testCase
                        "works with base 8"
                        (fun () ->
                            Assert.equal (consumeBase 8 0 "01234567", (8, 342391))
                            Assert.equal (consumeBase 8 0 "0123a4567", (4, 83))
                        )

                    testCase
                        "works with base 10"
                        (fun () ->
                            Assert.equal (consumeBase 10 0 "0123456789", (10, 123456789))
                            Assert.equal (consumeBase 10 0 "0123a456789", (4, 123))
                        )

                    testCase
                        "stops at the first non base symbol"
                        (fun () ->
                            Assert.equal (consumeBase 10 0 "0123a456789", (4, 123))
                            Assert.equal (consumeBase 10 0 "a0123456789", (0, 0))
                        )
                ]
        ]
