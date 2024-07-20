# Thoth.Parser

[![NuGet](https://img.shields.io/nuget/v/Thoth.Parser.svg)](https://www.nuget.org/packages/Thoth.Parser)
[![](https://img.shields.io/badge/Sponsors-EA4AAA)](https://mangelmaxime.github.io/sponsors/)

Common commit parser library used by other EasyBuild tools like [EasyBuild.ChangelogGen](https://github.com/easybuild-org/EasyBuild.ChangelogGen) or [EasyBuild.CommitLinter](https://github.com/easybuild-org/EasyBuild.CommitLinter).

Thoth.Parser is a parsing library aiming to provide a simple and efficient way to parse text.

It aims to be:

- **Terse**: The syntax should be as simple as possible
- **Speedy**: Go fast enough for most use cases
- **Modular**: Allow for easy extension and modification
- **Cross-platform**:

    *Thanks to [Fable](https://fable.io/), Thoth.Parser can be used on different runtime*

    - .NET ✅
    - JavaScript 🚧 (planned)
    - Python 🚧 (planned)

⚠️ Thoth.Parser is in early development and the API is subject to change.

```fs
open Thoth.Parser
open Thoth.Parser.Operators

type Point =
    {
        X: int
        Y: int
    }

module Point =

    let parser =
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
```

## Installation

You can install Thoth.Parser via NuGet:

```bash
dotnet add package Thoth.Parser
```

## Usage

### Pipeline VS Operators

Thoth.Parser provides two syntaxes:

- The pipeline syntax
    - `Parser.drop` to ignore the result of a parser
    - `Parser.keep` to keep the result of a parser

- The operator syntax
    - `|.` equivalent to `Parser.drop`
    - `|=` equivalent to `Parser.keep`

#### Pipeline example

```fs
open Thoth.Parser

Parser.succeed (fun x y ->
    {
        X = x
        Y = y
    }
)
|> Parser.drop (Parser.token "(")
|> Parser.drop Parser.spaces
|> Parser.keep Parser.int32
|> Parser.drop Parser.spaces
|> Parser.drop (Parser.token ",")
|> Parser.drop Parser.spaces
|> Parser.keep Parser.int32
|> Parser.drop Parser.spaces
|> Parser.drop (Parser.token ")")
|> Parser.drop Parser.exhausted
```

As you can see, thanks to `drop` and `keep`, being short and 4 characters long, the code is aligned and easy to read.

> [!TIP]
> To make sure the code is aligned, it is recommended to use a monospaced font.

#### Operators example

```fs
open Thoth.Parser
open Thoth.Parser.Operators

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
```

If you are using [Fantomas](https://fsprojects.github.io/fantomas/) to format your F# code, it can happens that Fantomas will format the code in a single line.

```fs
let number =
    Parser.succeed Number.Create
    |= Parser.sign
    |= Parser.int32

// will be formatted as

let number =
    Parser.succeed Number.Create |= Parser.sign |= Parser.int32
```

To prevent this, you can use [`fsharp_max_infix_operator_expression`](https://fsprojects.github.io/fantomas/docs/end-users/Configuration.html#fsharp_max_infix_operator_expression) setting to minimize the chance of this happening.

Another solution is to add comments between the operators.

```fs
let number =
    Parser.succeed Number.Create
    //
    |= Parser.sign
    //
    |= Parser.int32
```
