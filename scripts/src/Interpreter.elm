module Interpreter exposing (Value, findUnreachableCalls, visitExpression)

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import List.Nonempty as Nonempty exposing (Nonempty)


type Value
    = Int_ Int
    | Float_ Float
    | String_ String
    | Tuple_ (List Value)
    | List_ (List Value)
    | Record (Dict String Value)
    | Function_ ModuleName String (List Value)
    | AnonymousFunction_ Expression (List Value)


visitorToList : (Node Expression -> Maybe a) -> Node Expression -> List ( Nonempty (Node Expression), a )
visitorToList visitor expression =
    case visitor expression of
        Just a ->
            [ ( Nonempty.fromElement expression, a ) ]

        Nothing ->
            []


findUnreachableCalls : File -> List ( Nonempty (Node Expression), () )
findUnreachableCalls file =
    file.declarations
        |> List.filterMap
            (\(Node _ a) ->
                case a of
                    FunctionDeclaration declaration ->
                        declaration.declaration
                            |> Node.value
                            |> .expression
                            |> visitExpression
                                (\(Node _ expression) ->
                                    case expression of
                                        Application [ Node _ (FunctionOrValue _ "unreachableCase"), Node _ UnitExpr ] ->
                                            Just ()

                                        _ ->
                                            Nothing
                                )
                            |> Just

                    _ ->
                        Nothing
            )
        |> List.concat


visitExpression : (Node Expression -> Maybe a) -> Node Expression -> List ( Nonempty (Node Expression), a )
visitExpression visitor node =
    (case Node.value node of
        UnitExpr ->
            []

        Application nodes ->
            List.concatMap (visitExpression visitor) nodes

        OperatorApplication _ _ left right ->
            visitExpression visitor left ++ visitExpression visitor right

        FunctionOrValue _ _ ->
            []

        IfBlock condition ifTrue ifFalse ->
            visitExpression visitor condition
                ++ visitExpression visitor ifTrue
                ++ visitExpression visitor ifFalse

        PrefixOperator _ ->
            []

        Operator _ ->
            []

        Integer _ ->
            []

        Hex _ ->
            []

        Floatable _ ->
            []

        Negation node_ ->
            visitExpression visitor node_

        Literal _ ->
            []

        CharLiteral _ ->
            []

        TupledExpression nodes ->
            List.concatMap (visitExpression visitor) nodes

        ParenthesizedExpression node_ ->
            visitExpression visitor node_

        LetExpression letBlock ->
            List.concatMap
                (\(Node _ declaration) ->
                    case declaration of
                        LetFunction function ->
                            function.declaration |> Node.value |> .expression |> visitExpression visitor

                        LetDestructuring _ letFunction ->
                            visitExpression visitor letFunction
                )
                letBlock.declarations
                ++ visitExpression visitor letBlock.expression

        CaseExpression caseBlock ->
            visitExpression visitor caseBlock.expression
                ++ List.concatMap (Tuple.second >> visitExpression visitor) caseBlock.cases

        LambdaExpression lambda ->
            visitExpression visitor lambda.expression

        RecordExpr nodes ->
            List.concatMap (Node.value >> Tuple.second >> visitExpression visitor) nodes

        ListExpr nodes ->
            List.concatMap (visitExpression visitor) nodes

        RecordAccess node_ _ ->
            visitExpression visitor node_

        RecordAccessFunction _ ->
            []

        RecordUpdateExpression _ nodes ->
            List.concatMap (Node.value >> Tuple.second >> visitExpression visitor) nodes

        GLSLExpression _ ->
            []
    )
        |> List.map (Tuple.mapFirst (Nonempty.cons node))
        |> (++) (visitorToList visitor node)
