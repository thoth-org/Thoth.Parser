module Thoth.Parser.Base.Numbers

open Thoth.Parser.Base
open Thoth.Parser.LowLevel
open System.Text

[<RequireQualifiedAccess>]
type Sign =
    | Positive
    | Negative

module Parser =

    /// <summary>
    /// Try to parse a sign from the input text.
    /// </summary>
    /// <param name="invalidSign">Error to report if the sign is invalid.</param>
    /// <returns>
    /// Returns <c>Sign.Negative</c> if the sign is a minus sign, <c>Sign.Positive</c> otherwise.
    /// </returns>
    let sign (invalidSign: 'Problem) : Parser<'Context, 'Problem, Sign> =
        ParserFunc
        <| fun state ->
            let minusOffset = charMatchAt ((=) (Rune('-'))) state.Offset state.Source

            match minusOffset with
            | CharMatchAtResult.Match newOffset ->
                ParserStep.Success
                    {
                        Backtrackable = true
                        Value = Sign.Negative
                        State = Parser.bumpOffset newOffset state
                    }
            | CharMatchAtResult.NoMatch ->
                ParserStep.Success
                    {
                        Backtrackable = true
                        Value = Sign.Positive
                        State = state
                    }
            | CharMatchAtResult.NewLine ->
                ParserStep.Failed
                    {
                        Backtrackable = false
                        Bag = Parser.fromState state invalidSign
                    }

    let inline private signedIntegral
        (invalidSign: 'Problem)
        (invalidNumber: 'Problem)
        (cast: uint64 -> 'T)
        (maxValue: uint64)
        (negativeMultiplier: 'T)
        : Parser<'Context, 'Problem, 'T>
        =
        Parser.succeed (fun (sign: Sign) (digits: string) -> sign, digits)
        |> Parser.keep (sign invalidSign)
        |> Parser.keep Parser.digits
        |> Parser.andThen (fun (sign, digits) ->
            ParserFunc
            <| fun state ->
                if digits.Length = 0 then
                    ParserStep.Failed
                        {
                            Backtrackable = false
                            Bag = Parser.fromState state invalidNumber
                        }
                else
                    match System.UInt64.TryParse digits with
                    | false, _ ->
                        ParserStep.Failed
                            {
                                Backtrackable = false
                                Bag = Parser.fromState state invalidNumber
                            }
                    | true, value ->
                        let isNegative = sign = Sign.Negative

                        if value <= maxValue || isNegative && value = maxValue + 1UL then
                            let value =
                                match sign with
                                | Sign.Positive -> cast value
                                | Sign.Negative -> negativeMultiplier * cast value

                            ParserStep.Success
                                {
                                    Backtrackable = true
                                    Value = value
                                    State = state
                                }
                        else
                            ParserStep.Failed
                                {
                                    Backtrackable = false
                                    Bag = Parser.fromState state invalidNumber
                                }

        )

    /// <summary>
    /// Parse a signed 8-bit integer from the input text.
    ///
    /// Only supports base 10 numbers.
    /// </summary>
    /// <param name="invalidSign">Problem to report if the sign is invalid.</param>
    /// <param name="invalidNumber">Problem to report if the number is invalid.</param>
    /// <returns>
    /// Returns a parser that consumes a signed 8-bit integer.
    /// </returns>
    let int8 (invalidSign: 'Problem) (invalidNumber: 'Problem) : Parser<'Context, 'Problem, sbyte> =
        signedIntegral invalidSign invalidNumber int8 127UL -1y

    /// <summary>
    /// Parse a signed 16-bit integer from the input text.
    ///
    /// Only supports base 10 numbers.
    /// </summary>
    /// <param name="invalidSign">Problem to report if the sign is invalid.</param>
    /// <param name="invalidNumber">Problem to report if the number is invalid.</param>
    /// <returns>
    /// Returns a parser that consumes a signed 16-bit integer.
    /// </returns>
    let int16
        (invalidSign: 'Problem)
        (invalidNumber: 'Problem)
        : Parser<'Context, 'Problem, int16>
        =
        signedIntegral invalidSign invalidNumber int16 32767UL -1s

    /// <summary>
    /// Parse a signed 32-bit integer from the input text.
    ///
    /// Only supports base 10 numbers.
    /// </summary>
    /// <param name="invalidSign">Problem to report if the sign is invalid.</param>
    /// <param name="invalidNumber">Problem to report if the number is invalid.</param>
    /// <returns>
    /// Returns a parser that consumes a signed 32-bit integer.
    /// </returns>
    let int32 (invalidSign: 'Problem) (invalidNumber: 'Problem) : Parser<'Context, 'Problem, int> =
        signedIntegral invalidSign invalidNumber int 2147483647UL -1

    /// <summary>
    /// Parse a signed 64-bit integer from the input text.
    ///
    /// Only supports base 10 numbers.
    /// </summary>
    /// <param name="invalidSign">Problem to report if the sign is invalid.</param>
    /// <param name="invalidNumber">Problem to report if the number is invalid.</param>
    /// <returns>
    /// Returns a parser that consumes a signed 64-bit integer.
    /// </returns>
    let int64
        (invalidSign: 'Problem)
        (invalidNumber: 'Problem)
        : Parser<'Context, 'Problem, int64>
        =
        signedIntegral invalidSign invalidNumber int64 9223372036854775807UL -1L

    /// <summary>
    /// Parse an unsigned 8-bit integer from the input text.
    ///
    /// Only supports base 10 numbers.
    /// </summary>
    /// <param name="invalidNumber">Problem to report if the number is invalid.</param>
    /// <returns>
    /// Returns a parser that consumes an unsigned 8-bit integer.
    /// </returns>
    let uint8 (invalidNumber: 'Problem) : Parser<'Context, 'Problem, uint8> =
        Parser.digits
        |> Parser.andThen (fun value ->
            match System.Byte.TryParse value with
            | true, value -> Parser.commit value
            | false, _ -> Parser.problem invalidNumber
        )

    /// <summary>
    /// Parse an unsigned 16-bit integer from the input text.
    ///
    /// Only supports base 10 numbers.
    /// </summary>
    /// <param name="invalidNumber">Problem to report if the number is invalid.</param>
    /// <returns>
    /// Returns a parser that consumes an unsigned 16-bit integer.
    /// </returns>
    let uint16 (invalidNumber: 'Problem) : Parser<'Context, 'Problem, uint16> =
        Parser.digits
        |> Parser.andThen (fun value ->
            match System.UInt16.TryParse value with
            | true, value -> Parser.commit value
            | false, _ -> Parser.problem invalidNumber
        )

    /// <summary>
    /// Parse an unsigned 32-bit integer from the input text.
    ///
    /// Only supports base 10 numbers.
    /// </summary>
    /// <param name="invalidNumber">Problem to report if the number is invalid.</param>
    /// <returns>
    /// Returns a parser that consumes an unsigned 32-bit integer.
    /// </returns>
    let uint32 (invalidNumber: 'Problem) : Parser<'Context, 'Problem, uint> =
        Parser.digits
        |> Parser.andThen (fun value ->
            match System.UInt32.TryParse value with
            | true, value -> Parser.commit value
            | false, _ -> Parser.problem invalidNumber
        )

    /// <summary>
    /// Parse an unsigned 64-bit integer from the input text.
    ///
    /// Only supports base 10 numbers.
    /// </summary>
    /// <param name="invalidNumber">Problem to report if the number is invalid.</param>
    /// <returns>
    /// Returns a parser that consumes an unsigned 64-bit integer.
    /// </returns>
    let uint64 (invalidNumber: 'Problem) : Parser<'Context, 'Problem, uint64> =
        Parser.digits
        |> Parser.andThen (fun value ->
            match System.UInt64.TryParse value with
            | true, value -> Parser.commit value
            | false, _ -> Parser.problem invalidNumber
        )
