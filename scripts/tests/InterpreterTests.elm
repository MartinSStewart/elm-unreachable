module InterpreterTests exposing (..)

import Elm.Parser
import Elm.Processing
import Elm.Syntax.File exposing (File)
import Expect
import Interpreter
import Test exposing (..)
import Unreachable


simpleModule : File
simpleModule =
    """
module A exposing (..)

import Unreachable exposing (unreachableCase)

a =
    case True of
        True -> ()
        False -> unreachableCase ()
"""
        |> String.trim
        |> String.replace "\u{000D}\n" "\n"
        |> Elm.Parser.parse
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Unreachable.alwaysOk


findUnreachableTest =
    test "Find unreachable call" <|
        \_ ->
            Interpreter.findUnreachableCalls simpleModule
                |> Expect.equal []
