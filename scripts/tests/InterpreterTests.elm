module InterpreterTests exposing (..)

import Elm.Parser
import Elm.Processing
import Expect
import Interpreter exposing (CallTree(..))
import Test exposing (..)
import Unreachable


parseCode =
    String.replace "\u{000D}\n" "\n"
        >> Elm.Parser.parse
        >> Result.map (Elm.Processing.process Elm.Processing.init)
        >> Unreachable.alwaysOk


codeTest : String -> String -> List Interpreter.Reachability -> Test
codeTest name file expected =
    test name <|
        \_ ->
            parseCode file
                |> Interpreter.visitFile
                |> Interpreter.visitTree
                |> Expect.equal expected


tests : Test
tests =
    describe "Interpreter tests"
        [ codeTest "Is unreachable"
            """module A exposing (..)

import Unreachable exposing (unreachable)

a =
    case True of
        True -> ()
        False -> unreachable ()
"""
            [ Interpreter.Unreachable ]
        , codeTest "Is reachable"
            """module A exposing (..)

import Unreachable exposing (unreachable)

a =
    case False of
        True -> ()
        False -> unreachable ()
"""
            [ Interpreter.Reachable ]
        , codeTest "Is unreachable 2"
            """module A exposing (..)

import Unreachable exposing (unreachable)

a =
    case 2 + 5 of
        6 -> unreachable ()
        _ -> ()
"""
            [ Interpreter.Unreachable ]
        , codeTest "Is reachable 2"
            """module A exposing (..)

import Unreachable exposing (unreachable)

a =
    case 2 + 5 of
        7 -> unreachable ()
        _ -> ()
"""
            [ Interpreter.Reachable ]
        , codeTest "At least one is reachable"
            """module A exposing (..)

import Unreachable exposing (unreachable)

a =
    case 2 of
        7 -> unreachable ()
        2 -> unreachable ()
        _ -> ()
"""
            [ Interpreter.Reachable ]
        , codeTest "Is nested case block unreachable"
            """module A exposing (..)

import Unreachable exposing (unreachable)

a =
    case True of 
        True -> ()
        False ->
            case False of
                True -> ()
                False -> unreachable ()
"""
            [ Interpreter.Unreachable ]
        , codeTest "Is nested case block reachable"
            """module A exposing (..)

import Unreachable exposing (unreachable)

a =
    case False of 
        True -> ()
        False ->
            case False of
                True -> ()
                False -> unreachable ()
"""
            [ Interpreter.Reachable ]
        , codeTest "Is function call unreachable"
            """module A exposing (..)

import Unreachable exposing (unreachable)

b = True

a =
    case b of 
        True -> ()
        False -> unreachable ()
"""
            [ Interpreter.Unreachable ]
        , codeTest "Is function call reachable"
            """module A exposing (..)

import Unreachable exposing (unreachable)

b = False

a =
    case b of 
        True -> ()
        False -> unreachable ()
"""
            [ Interpreter.Reachable ]
        , codeTest "Is function call with param unreachable"
            """module A exposing (..)

import Unreachable exposing (unreachable)

b x = x

a =
    case b 4 of 
        4 -> ()
        _ -> unreachable ()
"""
            [ Interpreter.Unreachable ]
        , codeTest "Is function call with param reachable"
            """module A exposing (..)

import Unreachable exposing (unreachable)

b x = x

a =
    case b 4 of 
        5 -> ()
        _ -> unreachable ()
"""
            [ Interpreter.Reachable ]
        , codeTest "Is function call with 2 params reachable"
            """module A exposing (..)

import Unreachable exposing (unreachable)

b x y = x

a =
    case b 4 5 of
        5 -> ()
        _ -> unreachable ()
"""
            [ Interpreter.Reachable ]
        , codeTest "Is function call with 2 params unreachable"
            """module A exposing (..)

import Unreachable exposing (unreachable)

b x y = y

a =
    case b 4 5 of
        5 -> ()
        _ -> unreachable ()
"""
            [ Interpreter.Unreachable ]
        ]
