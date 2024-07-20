module Thoth.Parser.Tests.Base.Common

open Fable.Pyxpecto
open Thoth.Parser.Base
open Thoth.Parser.Tests.Prelude
open System.Text

[<RequireQualifiedAccess>]
type Problem =
    | Expecting of string
    | ExpectingEnd
    | UnexpectedChar

/// <summary>
/// Helpers function to easily create a Token
///
/// This is equivalent to the function in the Simple module, but this Test file is about
/// testing the Base module, so I don't want to import the Simple module.
///
/// I want this test file to be representative of how people would use the Base module.
/// </summary>
/// <param name="str"></param>
/// <returns></returns>
let mkToken (str: string) = Token(str, Problem.Expecting str)

let tests =
    [
        testList
            "chompUntil"
            [

                test "return Ok if the searched string is found" {
                    let token = mkToken "*)"
                    let actual = Parser.run (Parser.chompUntil token) "(* this is a comment *)"

                    Assert.equal (actual, Ok())
                }

                test "return an Error if the searched token is not found" {
                    let token = mkToken "*)"
                    let actual = Parser.run (Parser.chompUntil token) "(* this is a comment"

                    Assert.equal (
                        actual,
                        [
                            {
                                Row = 1
                                Column = 21
                                Problem = Problem.Expecting "*)"
                                ContextStack = []
                            }
                        ]
                        |> Error
                    )
                }
            ]

        testList
            "chompIf"
            [

                test "return Ok if the parser succeeds" {
                    let parser = Parser.chompIf (fun _ -> true) Problem.UnexpectedChar

                    let actual = Parser.run parser "a"

                    Assert.equal (actual, Ok())
                }

                test "return an Error if the parser fails" {
                    let parser = Parser.chompIf (fun _ -> false) Problem.UnexpectedChar

                    let actual = Parser.run parser "A"

                    Assert.equal (
                        actual,
                        [
                            {
                                Row = 1
                                Column = 1
                                Problem = Problem.UnexpectedChar
                                ContextStack = []
                            }
                        ]
                        |> Error
                    )
                }

                test "new line is handled correctly" {
                    let (ParserFunc parse) =
                        Parser.chompIf (fun c -> c = Rune '\n') Problem.UnexpectedChar

                    let actual = parse (State.Initial<obj> "\nSecondLine")

                    Assert.equal (
                        actual,
                        ParserStep.Success
                            {
                                Backtrackable = true
                                Value = ()
                                State =
                                    {
                                        Source = "\nSecondLine"
                                        Offset = 1
                                        Indent = 0
                                        Context = []
                                        Row = 2
                                        Column = 1
                                    }
                            }
                    )
                }
            ]

        testList
            "chompWhile"
            [

                test "chomp until the predicate fails" {
                    let (ParserFunc parse) =
                        Parser.chompWhile (fun rune ->
                            rune.ToString().ToLowerInvariant() = rune.ToString()
                        )

                    let actual = parse (State.Initial<obj> "abcB")

                    Assert.equal (
                        actual,
                        ParserStep.Success
                            {
                                Backtrackable = true
                                Value = ()
                                State =
                                    {
                                        Source = "abcB"
                                        Offset = 3
                                        Indent = 0
                                        Context = []
                                        Row = 1
                                        Column = 4
                                    }
                            }
                    )
                }

                test "chomp until the end of the string" {
                    let (ParserFunc parse) = Parser.chompWhile (fun _ -> true)

                    let actual = parse (State.Initial<obj> "abc")

                    Assert.equal (
                        actual,
                        ParserStep.Success
                            {
                                Backtrackable = true
                                Value = ()
                                State =
                                    {
                                        Source = "abc"
                                        Offset = 3
                                        Indent = 0
                                        Context = []
                                        Row = 1
                                        Column = 4
                                    }
                            }
                    )
                }

                test "support new line" {
                    let (ParserFunc parse) =
                        Parser.chompWhile (fun rune ->
                            rune.ToString().ToLowerInvariant() = rune.ToString()
                            || rune = Rune '\n'
                        )

                    let actual = parse (State.Initial<obj> "abc\ndef\nA")

                    Assert.equal (
                        actual,
                        ParserStep.Success
                            {
                                Backtrackable = true
                                Value = ()
                                State =
                                    {
                                        Source = "abc\ndef\nA"
                                        Offset = 8
                                        Indent = 0
                                        Context = []
                                        Row = 3
                                        Column = 1
                                    }
                            }
                    )
                }
            ]

        testList
            "exhausted"
            [

                test "returns success if the parser is at the end of the string" {
                    let (ParserFunc parse) = Parser.exhausted Problem.ExpectingEnd

                    let actual =
                        parse
                            {
                                Source = "abc"
                                Offset = 3
                                Indent = 0
                                Context = []
                                Row = 1
                                Column = 4
                            }

                    Assert.equal (
                        actual,
                        ParserStep.Success
                            {
                                Backtrackable = false
                                Value = ()
                                State =
                                    {
                                        Source = "abc"
                                        Offset = 3
                                        Indent = 0
                                        Context = []
                                        Row = 1
                                        Column = 4
                                    }
                            }
                    )
                }

                test "returns success on empty string" {
                    let (ParserFunc parse) = Parser.exhausted Problem.ExpectingEnd

                    let actual = parse (State.Initial<obj> "")

                    Assert.equal (
                        actual,
                        ParserStep.Success
                            {
                                Backtrackable = false
                                Value = ()
                                State =
                                    {
                                        Source = ""
                                        Offset = 0
                                        Indent = 0
                                        Context = []
                                        Row = 1
                                        Column = 1
                                    }
                            }
                    )
                }

                test "returns an error if the parser is not at the end of the string" {
                    let (ParserFunc parse) = Parser.exhausted Problem.ExpectingEnd

                    let actual =
                        parse
                            {
                                Source = "abc"
                                Offset = 2
                                Indent = 0
                                Context = []
                                Row = 1
                                Column = 3
                            }

                    Assert.equal (
                        actual,
                        ParserStep.Failed
                            {
                                Backtrackable = false
                                Bag =
                                    AddRight(
                                        Empty,
                                        {
                                            Row = 1
                                            Column = 3
                                            Problem = Problem.ExpectingEnd
                                            ContextStack = []
                                        }
                                    )
                            }
                    )
                }

            ]

        testList
            "bumpOffset"
            [
                test "works for positive offset" {
                    let actual =
                        Parser.bumpOffset
                            5
                            {
                                Source = ""
                                Offset = 15
                                Indent = 0
                                Context = []
                                Row = 1
                                Column = 16
                            }

                    Assert.equal (
                        actual,
                        {
                            Source = ""
                            Offset = 5
                            Indent = 0
                            Context = []
                            Row = 1
                            Column = 6
                        }
                    )
                }
            ]

        testList
            "token"
            [
                test "works with a single character" {
                    let (ParserFunc parse) = Parser.token (mkToken "(*")

                    let actual = parse (State.Initial<obj> "(* This is a comment *)")

                    Assert.equal (
                        actual,
                        ParserStep.Success
                            {
                                Backtrackable = true
                                Value = ()
                                State =
                                    {
                                        Source = "(* This is a comment *)"
                                        Offset = 2
                                        Indent = 0
                                        Context = []
                                        Row = 1
                                        Column = 3
                                    }
                            }
                    )
                }

                test "doesn't progress if the token is not found" {
                    let (ParserFunc parse) = Parser.token (mkToken "(*")

                    let actual = parse (State.Initial<obj> "This is a comment *)")

                    Assert.equal (
                        actual,
                        ParserStep.Failed
                            {
                                Backtrackable = false
                                Bag =
                                    AddRight(
                                        Empty,
                                        {
                                            Row = 1
                                            Column = 1
                                            Problem = Problem.Expecting "(*"
                                            ContextStack = []
                                        }
                                    )
                            }
                    )
                }
            ]

        testList
            "backtrackable"
            [
                test "returns the value if the parser is successful" {
                    let (ParserFunc parse) = Parser.backtrackable (Parser.token (mkToken "(*"))

                    let actual = parse (State.Initial<obj> "(* This is a comment *)")

                    Assert.equal (
                        actual,
                        ParserStep.Success
                            {
                                Backtrackable = false
                                Value = ()
                                State =
                                    {
                                        Source = "(* This is a comment *)"
                                        Offset = 2
                                        Indent = 0
                                        Context = []
                                        Row = 1
                                        Column = 3
                                    }
                            }
                    )
                }
            ]

        testList
            "getChompedString"
            [
                test "returns the chomped string" {
                    let (ParserFunc parse) =
                        Parser.getChompedString (Parser.chompWhile (fun rune -> rune = Rune 'a'))

                    let actual = parse (State.Initial<obj> "aaaaaabb")

                    Assert.equal (
                        actual,
                        ParserStep.Success
                            {
                                Backtrackable = true
                                Value = "aaaaaa"
                                State =
                                    {
                                        Source = "aaaaaabb"
                                        Offset = 6
                                        Indent = 0
                                        Context = []
                                        Row = 1
                                        Column = 7
                                    }
                            }
                    )
                }
            ]

        testList
            "chumpUntilEndOr"
            [
                test
                    "chomp all the characters until the end of the string if the subString is not found" {
                    let (ParserFunc parse) = Parser.chumpUntilEndOr "*"

                    let actual = parse (State.Initial<obj> "This is a string of characters")

                    Assert.equal (
                        actual,
                        ParserStep.Success
                            {
                                Backtrackable = true
                                Value = ()
                                State =
                                    {
                                        Source = "This is a string of characters"
                                        Offset = 30
                                        Indent = 0
                                        Context = []
                                        Row = 1
                                        Column = 31
                                    }
                            }
                    )
                }

                test "chomp all the characters until the subString is found" {
                    let (ParserFunc parse) = Parser.chumpUntilEndOr "*"

                    let actual = parse (State.Initial<obj> "This is a string * of characters")

                    Assert.equal (
                        actual,
                        ParserStep.Success
                            {
                                Backtrackable = true
                                Value = ()
                                State =
                                    {
                                        Source = "This is a string * of characters"
                                        Offset = 18
                                        Indent = 0
                                        Context = []
                                        Row = 1
                                        Column = 19
                                    }
                            }
                    )
                }
            ]

        testList
            "keyword"
            [
                test "rejects if the keywoard is part of a word" {
                    let (ParserFunc parse) = Parser.keyword (mkToken "let")

                    let actual = parse (State.Initial<obj> "letter")

                    Assert.equal (
                        actual,
                        ParserStep.Failed
                            {
                                Backtrackable = false
                                Bag =
                                    AddRight(
                                        Empty,
                                        {
                                            Row = 1
                                            Column = 1
                                            Problem = Problem.Expecting "let"
                                            ContextStack = []
                                        }
                                    )
                            }
                    )
                }

                test "accepts if the keyword is the only word" {
                    let (ParserFunc parse) = Parser.keyword (mkToken "let")

                    let actual = parse (State.Initial<obj> "let")

                    Assert.equal (
                        actual,
                        ParserStep.Success
                            {
                                Backtrackable = true
                                Value = ()
                                State =
                                    {
                                        Source = "let"
                                        Offset = 3
                                        Indent = 0
                                        Context = []
                                        Row = 1
                                        Column = 4
                                    }
                            }
                    )
                }

                test "accepts if the keyword is at the beginning of the string" {
                    let (ParserFunc parse) = Parser.keyword (mkToken "let")

                    let actual = parse (State.Initial<obj> "let ter = 42")

                    Assert.equal (
                        actual,
                        ParserStep.Success
                            {
                                Backtrackable = true
                                Value = ()
                                State =
                                    {
                                        Source = "let ter = 42"
                                        Offset = 3
                                        Indent = 0
                                        Context = []
                                        Row = 1
                                        Column = 4
                                    }
                            }
                    )
                }
            ]
    ]
    |> testList "Base"
