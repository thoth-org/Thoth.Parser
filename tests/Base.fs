module Thoth.Parser.Tests.Base

open Fable.Pyxpecto
open Thoth.Parser.Base

[<RequireQualifiedAccess>]
type Problem =
    | Expecting of string
    | ExpectingEnd
    | UnexpectedChar

let mkToken (str: string) = Token(str, Problem.Expecting str)

let tests =
    [
        testList
            "chompUntil"
            [

                testCase
                    "return Ok if the searched string is found"
                    (fun () ->
                        let token = mkToken "*)"
                        let actual = Parser.run (Parser.chompUntil token) "(* this is a comment *)"

                        Assert.equal (actual, Ok())
                    )

                testCase
                    "return an Error if the searched token is not found"
                    (fun () ->
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
                    )
            ]

        testList
            "chompIf"
            [

                testCase
                    "return Ok if the parser succeeds"
                    (fun () ->
                        let parser =
                            Parser.chompIf
                                (fun c -> c.ToLowerInvariant() = c)
                                Problem.UnexpectedChar

                        let actual = Parser.run parser "a"

                        Assert.equal (actual, Ok())
                    )

                testCase
                    "return an Error if the parser fails"
                    (fun () ->
                        let parser =
                            Parser.chompIf
                                (fun c -> c.ToLowerInvariant() = c)
                                Problem.UnexpectedChar

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
                    )

                testCase
                    "new line is handled correctly"
                    (fun () ->
                        let (Parser parse) =
                            Parser.chompIf (fun c -> c = "\n") Problem.UnexpectedChar

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
                    )
            ]

        testList
            "chompWhile"
            [

                testCase
                    "chomp until the predicate fails"
                    (fun () ->
                        let (Parser parse) = Parser.chompWhile (fun c -> c.ToLowerInvariant() = c)

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
                    )

                testCase
                    "chomp until the end of the string"
                    (fun () ->
                        let (Parser parse) = Parser.chompWhile (fun c -> c.ToLowerInvariant() = c)

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
                    )

                testCase
                    "support new line"
                    (fun () ->
                        let (Parser parse) =
                            Parser.chompWhile (fun c -> c.ToLowerInvariant() = c || c = "\n")

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
                    )
            ]

        testList
            "exhausted"
            [

                test "returns success if the parser is at the end of the string" {
                    let (Parser parse) = Parser.exhausted Problem.ExpectingEnd

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
                    let (Parser parse) = Parser.exhausted Problem.ExpectingEnd

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
                    let (Parser parse) = Parser.exhausted Problem.ExpectingEnd

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
                testCase
                    "works for positive offset"
                    (fun () ->
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
                    )
            ]

        testList
            "token"
            [
                testCase
                    "works with a single character"
                    (fun () ->
                        let (Parser parse) = Parser.token (mkToken "(*")

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
                    )

                testCase
                    "doesn't progress if the token is not found"
                    (fun () ->
                        let (Parser parse) = Parser.token (mkToken "(*")

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
                    )
            ]

        testList
            "backtrackable"
            [
                testCase
                    "returns the value if the parser is successful"
                    (fun () ->
                        let (Parser parse) = Parser.backtrackable (Parser.token (mkToken "(*"))

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
                    )
            ]

        testList
            "getChompedString"
            [
                testCase
                    "returns the chomped string"
                    (fun () ->
                        let (Parser parse) =
                            Parser.getChompedString (Parser.chompWhile (fun c -> c = "a"))

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
                    )
            ]

        testList
            "chumpUntilEndOr"
            [
                testCase
                    "chomp all the characters until the end of the string if the subString is not found"
                    (fun () ->
                        let (Parser parse) = Parser.chumpUntilEndOr "*"

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
                    )

                testCase
                    "chomp all the characters until the subString is found"
                    (fun () ->
                        let (Parser parse) = Parser.chumpUntilEndOr "*"

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
                    )
            ]

        testList
            "keyword"
            [
                testCase
                    "rejects if the keywoard is part of a word"
                    (fun () ->
                        let (Parser parse) = Parser.keyword (mkToken "let")

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
                    )

                testCase
                    "accepts if the keyword is the only word"
                    (fun () ->
                        let (Parser parse) = Parser.keyword (mkToken "let")

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
                    )

                testCase
                    "accepts if the keyword is at the beginning of the string"
                    (fun () ->
                        let (Parser parse) = Parser.keyword (mkToken "let")

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
                    )
            ]

        ftestList
            "int"
            [
                // testCase "parses a single digit" (fun () ->
                //     let (Parser parse) = int "Invalid number"

                //     let actual = parse (State.Initial<obj> "1")

                //     Assert.equal (
                //         actual,
                //         ParserStep.Success
                //             {
                //                 Backtrackable = true
                //                 Value = 1
                //                 State =
                //                     {
                //                         Source = "1"
                //                         Offset = 1
                //                         Indent = 0
                //                         Context = []
                //                         Row = 1
                //                         Column = 2
                //                     }
                //             }
                //     )
                // )

                // testCase "parses a multi digit number" (fun () ->
                //     let (Parser parse) = int "Invalid number"

                //     let actual = parse (State.Initial<obj> "123")

                //     Assert.equal (
                //         actual,
                //         ParserStep.Success
                //             {
                //                 Backtrackable = true
                //                 Value = 123
                //                 State =
                //                     {
                //                         Source = "123"
                //                         Offset = 3
                //                         Indent = 0
                //                         Context = []
                //                         Row = 1
                //                         Column = 4
                //                     }
                //             }
                //     )
                // )

                testCase
                    "parses a number with a sign"
                    (fun () ->
                        let (Parser parse) = Parser.int32 "Invalid sign"

                        let actual = parse (State.Initial<obj> "-123")

                        Assert.equal (
                            actual,
                            ParserStep.Success
                                {
                                    Backtrackable = true
                                    Value = -123
                                    State =
                                        {
                                            Source = "-123"
                                            Offset = 4
                                            Indent = 0
                                            Context = []
                                            Row = 1
                                            Column = 5
                                        }
                                }
                        )
                    )
            ]
    ]
    |> testList "Base"
