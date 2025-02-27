module EasyBuild.Commands.Test

open Spectre.Console.Cli
open SimpleExec
open EasyBuild.Workspace
open BlackFox.CommandLine

type TestSettings() =
    inherit CommandSettings()

    [<CommandOption("-w|--watch")>]
    member val IsWatch = false with get, set

type TestCommand() =
    inherit Command<TestSettings>()
    interface ICommandLimiter<TestSettings>

    override __.Execute(context, settings) =
        if settings.IsWatch then
            Command.Run(
                "dotnet",
                CmdLine.empty
                |> CmdLine.appendRaw "watch"
                |> CmdLine.appendRaw "run"
                |> CmdLine.appendPrefix "--project" Workspace.tests.``Thoth.Parser.Tests.fsproj``
                |> CmdLine.toString
            )
        else
            Command.Run(
                "dotnet",
                CmdLine.empty
                |> CmdLine.appendRaw "run"
                |> CmdLine.appendPrefix "--project" Workspace.tests.``Thoth.Parser.Tests.fsproj``
                |> CmdLine.toString
            )

        0
