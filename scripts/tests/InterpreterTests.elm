module InterpreterTests exposing (..)

import Elm.Parser
import Elm.Processing
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Expect
import Interpreter
import List.Nonempty exposing (Nonempty(..))
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
                |> Expect.equal
                    [ ( Nonempty
                            (Node { end = { column = 36, row = 8 }, start = { column = 5, row = 6 } }
                                (CaseExpression
                                    { cases =
                                        [ ( Node { end = { column = 13, row = 7 }, start = { column = 9, row = 7 } }
                                                (NamedPattern { moduleName = [], name = "True" } [])
                                          , Node { end = { column = 19, row = 7 }, start = { column = 17, row = 7 } } UnitExpr
                                          )
                                        , ( Node { end = { column = 14, row = 8 }, start = { column = 9, row = 8 } } (NamedPattern { moduleName = [], name = "False" } []), Node { end = { column = 36, row = 8 }, start = { column = 18, row = 8 } } (Application [ Node { end = { column = 33, row = 8 }, start = { column = 18, row = 8 } } (FunctionOrValue [] "unreachableCase"), Node { end = { column = 36, row = 8 }, start = { column = 34, row = 8 } } UnitExpr ]) )
                                        ]
                                    , expression = Node { end = { column = 14, row = 6 }, start = { column = 10, row = 6 } } (FunctionOrValue [] "True")
                                    }
                                )
                            )
                            [ Node { end = { column = 36, row = 8 }, start = { column = 18, row = 8 } } (Application [ Node { end = { column = 33, row = 8 }, start = { column = 18, row = 8 } } (FunctionOrValue [] "unreachableCase"), Node { end = { column = 36, row = 8 }, start = { column = 34, row = 8 } } UnitExpr ]) ]
                      , ()
                      )
                    ]
