namespace Thoth.Parser

open Thoth.Parser
open System.Text

[<RequireQualifiedAccess>]
type Problem =
    | Expecting of string
    | ExpectingEnd
    | UnexpectedChar

type DeadEnd =
    {
        Row: int
        Column: int
        Problem: Problem
    }

// TODO: What should be the type of 'Context ?
type Parser<'Value> = Base.Parser<obj, Problem, 'Value>

module Parser =

    let succeed: 'Value -> Parser<'Value> = Base.Parser.succeed

    let run (parser: Parser<'Value>) (text: string) : Result<'Value, DeadEnd list> =
        match Base.Parser.run parser text with
        | Ok value -> Ok value
        | Error problems ->
            problems
            |> List.map (fun problem ->
                {
                    Row = problem.Row
                    Column = problem.Column
                    Problem = problem.Problem
                }
            )
            |> Error

    let deadEndsToString (deadEnds: DeadEnd list) : string =
        deadEnds
        |> List.map (fun deadEnd ->
            let problemMsg =
                match deadEnd.Problem with
                | Problem.Expecting str -> $"Expecting %s{str}"
                | Problem.ExpectingEnd -> "Expecting end"
                | Problem.UnexpectedChar -> "Unexpected char"

            $"({deadEnd.Row}, {deadEnd.Column}): {problemMsg}"
        )
        |> String.concat "\n"

    let keeper = Base.Parser.keeper

    let ignorer = Base.Parser.ignorer

    let skip (ignoreParser: Parser<'Value>) (parser: Parser<'Value>) : Parser<'Value> =
        Base.Parser.skip ignoreParser parser

    let lazy' (thunk: (unit -> Parser<'Value>)) : Parser<'Value> = Base.Parser.lazy' thunk

    let andThen (func: 'A -> Parser<'B>) (parser: Parser<'A>) : Parser<'B> =
        Base.Parser.andThen func parser

    let problem (problem: Problem) : Parser<'Value> = Base.Parser.problem problem

    let oneOf (parsers: Parser<'Value> list) : Parser<'Value> = Base.Parser.oneOf parsers

    let map (func: 'A -> 'B) (parser: Parser<'A>) : Parser<'B> = Base.Parser.map func parser

    let map2 (func: 'A -> 'B -> 'C) (parserA: Parser<'A>) (parserB: Parser<'B>) : Parser<'C> =
        Base.Parser.map2 func parserA parserB

    let backtrackable (parser: Parser<'Value>) : Parser<'Value> = Base.Parser.backtrackable parser

    let commit (value: 'Value) : Parser<'Value> = Base.Parser.commit value

    let toToken (str: string) = Base.Token(str, Problem.Expecting str)

    let token (str: string) : Parser<unit> = Base.Parser.token (toToken str)

    type LoopStep<'State, 'Value> =
        | Loop of 'State
        | Done of 'Value

    let private toBaseLoopStep (step: LoopStep<'State, 'Value>) =
        match step with
        | Loop state -> Base.Parser.Loop state
        | Done value -> Base.Parser.Done value

    let loop (state: 'State) (func: 'State -> Parser<LoopStep<'State, 'Value>>) : Parser<'Value> =
        Base.Parser.loop state (fun state -> map toBaseLoopStep (func state))

    let exhausted: Parser<unit> = Base.Parser.exhausted Problem.ExpectingEnd

    let end': Parser<unit> = Base.Parser.end' Problem.ExpectingEnd

    let mapChompedString
        (func: string -> string -> 'Value)
        (parser: Parser<string>)
        : Parser<'Value>
        =
        Base.Parser.mapChompedString func parser

    let getChompedString (parser: Parser<'A>) : Parser<string> = Base.Parser.getChompedString parser

    let chompIf (predicate: Rune -> bool) : Parser<unit> =
        Base.Parser.chompIf predicate Problem.UnexpectedChar

    let chompWhile (predicate: Rune -> bool) : Parser<unit> = Base.Parser.chompWhile predicate

    let chompUntil (str: string) : Parser<unit> = Base.Parser.chompUntil (toToken str)

    let chumpUntilEndOr (str: string) : Parser<unit> = Base.Parser.chumpUntilEndOr str

    let getPosition: Parser<LowLevel.Position> = Base.Parser.getPosition

    let getRow: Parser<int> = Base.Parser.getRow

    let getColumn: Parser<int> = Base.Parser.getColumn

    let getOffset: Parser<int> = Base.Parser.getOffset

    let getSource: Parser<string> = Base.Parser.getSource

    let getIndent: Parser<int> = Base.Parser.getIndent

    let withIndent (newIndent: int) (parser: Parser<'Value>) : Parser<'Value> =
        Base.Parser.withIndent newIndent parser

    let whitespaces: Parser<unit> = Base.Parser.whitespaces

    let spaces: Parser<unit> = Base.Parser.spaces

    let keyword (str: string) : Parser<unit> = Base.Parser.keyword (toToken str)

[<AutoOpen>]
module Operators =

    let (|=) = Parser.keeper

    let (|.) = Parser.ignorer
