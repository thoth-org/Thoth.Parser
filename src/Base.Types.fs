namespace Thoth.Parser.Base

// We need to split the types and functions into separate files
// Otherwise, F# is confused when doing the following:
//
// open Thoth.Parser
//
// let succeed: 'Value -> Parser<'Value> = Base.Parser.succeed
//                                              ^^^^^
//                                              Here Parser does not refer the module Parser
//                                              but instead the DU Case Parser in the module Base
//                                              ¯\_(ツ)_/¯

type LocatedContext<'Context> =
    {
        Row: int
        Column: int
        Context: 'Context
    }

    static member inline Create (row: int) (column: int) (context: 'Context) =
        {
            Row = row
            Column = column
            Context = context
        }

type State<'Context> =
    {
        Source: string
        Offset: int
        Indent: int
        Context: LocatedContext<'Context> list
        Row: int
        Column: int
    }

    static member inline Initial(source: string) =
        {
            Source = source
            Offset = 0
            Indent = 0
            Context = []
            Row = 1
            Column = 1
        }

type DeadEnd<'Context, 'Problem> =
    {
        Row: int
        Column: int
        Problem: 'Problem
        ContextStack: LocatedContext<'Context> list // Should it be a separated type?
    }

    /// <summary>
    /// Helper function making it easier to create a DeadEnd instance.
    ///
    /// This function does not impact the performance because it is inlined.
    /// </summary>
    /// <param name="row"></param>
    /// <param name="column"></param>
    /// <param name="problem"></param>
    /// <param name="context"></param>
    /// <typeparam name="'Context"></typeparam>
    /// <typeparam name="'Problem"></typeparam>
    /// <returns>
    /// A DeadEnd instance.
    /// </returns>
    static member inline Create
        (row: int)
        (column: int)
        (problem: 'Problem)
        (context: LocatedContext<'Context> list)
        =
        {
            Row = row
            Column = column
            Problem = problem
            ContextStack = context
        }

type Bag<'Context, 'Problem> =
    | Empty
    | AddRight of Bag<'Context, 'Problem> * DeadEnd<'Context, 'Problem>
    | Append of Bag<'Context, 'Problem> * Bag<'Context, 'Problem>

[<RequireQualifiedAccess>]
module ParserStep =

    type Success<'Context, 'Value> =
        {
            Backtrackable: bool
            Value: 'Value
            State: State<'Context>
        }

    type Failed<'Context, 'Problem> =
        {
            Backtrackable: bool
            Bag: Bag<'Context, 'Problem>
        }

[<RequireQualifiedAccess>]
type ParserStep<'Context, 'Problem, 'Value> =
    | Success of ParserStep.Success<'Context, 'Value>
    | Failed of ParserStep.Failed<'Context, 'Problem>

type Parser<'Context, 'Problem, 'Value> =
    | Parser of (State<'Context> -> ParserStep<'Context, 'Problem, 'Value>)

type Token<'T> = Token of string * 'T
