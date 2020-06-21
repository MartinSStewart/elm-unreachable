module InterpreterTests exposing (..)

import Elm.Parser
import Elm.Processing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range as Range
import Expect
import Interpreter exposing (CallTree(..))
import List.Zipper exposing (Zipper)
import Test exposing (..)
import Unreachable


simpleModule : File
simpleModule =
    """module A exposing (..)

import Unreachable exposing (unreachable)

a =
    case True of
        True -> ()
        False -> unreachable ()
"""
        |> String.replace "\u{000D}\n" "\n"
        |> Elm.Parser.parse
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Unreachable.alwaysOk


simpleModuleReachable : File
simpleModuleReachable =
    """module A exposing (..)

import Unreachable exposing (unreachable)

a =
    case False of
        True -> ()
        False -> unreachable ()
"""
        |> String.replace "\u{000D}\n" "\n"
        |> Elm.Parser.parse
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Unreachable.alwaysOk


moduleAdditionUnreachable : File
moduleAdditionUnreachable =
    """module A exposing (..)

import Unreachable exposing (unreachable)

a =
    case 2 + 5 of
        6 -> unreachable ()
        _ -> ()
"""
        |> String.replace "\u{000D}\n" "\n"
        |> Elm.Parser.parse
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Unreachable.alwaysOk


moduleAdditionReachable : File
moduleAdditionReachable =
    """module A exposing (..)

import Unreachable exposing (unreachable)

a =
    case 2 + 5 of
        7 -> unreachable ()
        _ -> ()
"""
        |> String.replace "\u{000D}\n" "\n"
        |> Elm.Parser.parse
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Unreachable.alwaysOk


moduleAdditionBoth : File
moduleAdditionBoth =
    """module A exposing (..)

import Unreachable exposing (unreachable)

a =
    case 2 of
        7 -> unreachable ()
        2 -> unreachable ()
        _ -> ()
"""
        |> String.replace "\u{000D}\n" "\n"
        |> Elm.Parser.parse
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Unreachable.alwaysOk


moduleNestedCaseBlockUnreachable : File
moduleNestedCaseBlockUnreachable =
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
        |> String.replace "\u{000D}\n" "\n"
        |> Elm.Parser.parse
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Unreachable.alwaysOk


moduleNestedCaseBlockReachable : File
moduleNestedCaseBlockReachable =
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
        |> String.replace "\u{000D}\n" "\n"
        |> Elm.Parser.parse
        |> Result.map (Elm.Processing.process Elm.Processing.init)
        |> Unreachable.alwaysOk


removeExprFields : CallTree -> CallTree
removeExprFields tree =
    let
        emptyExpr =
            Node Range.emptyRange Expression.UnitExpr
    in
    case tree of
        Unreachable_ { dependsOn } ->
            { expr = emptyExpr, dependsOn = removeExprFields dependsOn } |> Unreachable_

        CasePattern { dependsOn, caseOfDependsOn, patterns } ->
            { expr = emptyExpr
            , dependsOn = removeExprFields dependsOn
            , caseOfDependsOn = caseOfDependsOn
            , patterns = List.Zipper.map (Node.value >> Node Range.emptyRange) patterns
            }
                |> CasePattern

        CaseOf { dependsOn } ->
            { expr = emptyExpr, dependsOn = removeExprFields dependsOn } |> CaseOf

        FunctionDeclaration_ { name } ->
            { expr = emptyExpr, name = name } |> FunctionDeclaration_


tests : Test
tests =
    describe "Interpreter tests"
        [ test "Is unreachable" <|
            \_ ->
                Interpreter.visitFile simpleModule
                    |> List.map Interpreter.visitTree
                    |> Expect.equal [ Interpreter.Unreachable ]
        , test "Is reachable" <|
            \_ ->
                Interpreter.visitFile simpleModuleReachable
                    |> List.map Interpreter.visitTree
                    |> Expect.equal [ Interpreter.Reachable ]
        , test "Is unreachable 2" <|
            \_ ->
                Interpreter.visitFile moduleAdditionUnreachable
                    |> List.map Interpreter.visitTree
                    |> Expect.equal [ Interpreter.Unreachable ]
        , test "Is reachable 2" <|
            \_ ->
                Interpreter.visitFile moduleAdditionReachable
                    |> List.map Interpreter.visitTree
                    |> Expect.equal [ Interpreter.Reachable ]
        , test "Is both" <|
            \_ ->
                Interpreter.visitFile moduleAdditionBoth
                    |> List.map Interpreter.visitTree
                    |> Expect.equal [ Interpreter.Unreachable, Interpreter.Reachable ]
        , test "Is nested case block unreachable" <|
            \_ ->
                Interpreter.visitFile moduleNestedCaseBlockUnreachable
                    |> List.map Interpreter.visitTree
                    |> Expect.equal [ Interpreter.Unreachable ]
        , test "Is nested case block reachable" <|
            \_ ->
                Interpreter.visitFile moduleNestedCaseBlockReachable
                    |> List.map Interpreter.visitTree
                    |> Expect.equal [ Interpreter.Reachable ]
        ]
