module Interpreter exposing (CallTree(..), Reachability(..), visitFile, visitTree)

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression as Expression exposing (Expression, LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range
import List.Extra
import List.Zipper exposing (Zipper)


type CallTree
    = Unreachable_ { expr : Node Expression, dependsOn : CallTree }
    | CasePattern { expr : Node Expression, dependsOn : CallTree, caseOfDependsOn : Node Expression, patterns : Zipper (Node Pattern) }
    | CaseOf { expr : Node Expression, dependsOn : CallTree }
    | FunctionDeclaration_ { expr : Node Expression, name : String }


type Reachability
    = Reachable
    | Unreachable
    | Unknown


type alias Scope =
    { topLevel : Dict String Expression.Function
    }


getBaseScope : File -> Scope
getBaseScope file =
    file.declarations
        |> List.filterMap
            (\(Node _ declaration) ->
                case declaration of
                    FunctionDeclaration function ->
                        Just
                            ( Node.value function.declaration |> .name |> Node.value
                            , function
                            )

                    _ ->
                        Nothing
            )
        |> Dict.fromList
        |> Scope


handleOperatorApplication : Scope -> Expression -> String -> Node Expression -> Node Expression -> Expression
handleOperatorApplication scope expression operator left right =
    let
        comparisonCheck : (Int -> Int -> Bool) -> (Float -> Float -> Bool) -> Expression -> Expression -> Expression
        comparisonCheck comparisonInt comparisonFloat c d =
            case ( c, d ) of
                ( Expression.Integer a, Expression.Integer b ) ->
                    if comparisonInt a b then
                        Expression.FunctionOrValue [] "True"

                    else
                        Expression.FunctionOrValue [] "False"

                ( Expression.Floatable a, Expression.Floatable b ) ->
                    if comparisonFloat a b then
                        Expression.FunctionOrValue [] "True"

                    else
                        Expression.FunctionOrValue [] "False"

                ( Expression.Integer a, Expression.Floatable b ) ->
                    if comparisonFloat (toFloat a) b then
                        Expression.FunctionOrValue [] "True"

                    else
                        Expression.FunctionOrValue [] "False"

                ( Expression.Floatable a, Expression.Integer b ) ->
                    if comparisonFloat a (toFloat b) then
                        Expression.FunctionOrValue [] "True"

                    else
                        Expression.FunctionOrValue [] "False"

                _ ->
                    expression

        numberMath : (Int -> Int -> Int) -> (Float -> Float -> Float) -> Expression -> Expression -> Expression
        numberMath intMath floatMath c d =
            case ( c, d ) of
                ( Expression.Integer a, Expression.Integer b ) ->
                    intMath a b |> Expression.Integer

                ( Expression.Floatable a, Expression.Floatable b ) ->
                    floatMath a b |> Expression.Floatable

                ( Expression.Integer a, Expression.Floatable b ) ->
                    floatMath (toFloat a) b |> Expression.Floatable

                ( Expression.Floatable a, Expression.Integer b ) ->
                    floatMath a (toFloat b) |> Expression.Floatable

                _ ->
                    expression
    in
    case operator of
        "|>" ->
            case Node.value right of
                Expression.Application application ->
                    application
                        ++ [ left ]
                        |> Expression.Application
                        |> Node Elm.Syntax.Range.emptyRange
                        |> simplifyExpression scope
                        |> Node.value

                _ ->
                    Debug.todo ""

        "<|" ->
            case Node.value left of
                Expression.Application application ->
                    application
                        ++ [ right ]
                        |> Expression.Application
                        |> Node Elm.Syntax.Range.emptyRange
                        |> simplifyExpression scope
                        |> Node.value

                _ ->
                    Debug.todo ""

        "+" ->
            numberMath (+)
                (+)
                (simplifyExpression scope left |> Node.value)
                (simplifyExpression scope right |> Node.value)

        "-" ->
            numberMath (-)
                (-)
                (simplifyExpression scope left |> Node.value)
                (simplifyExpression scope right |> Node.value)

        "*" ->
            numberMath (*)
                (*)
                (simplifyExpression scope left |> Node.value)
                (simplifyExpression scope right |> Node.value)

        "//" ->
            case ( simplifyExpression scope left |> Node.value, simplifyExpression scope right |> Node.value ) of
                ( Expression.Integer a, Expression.Integer b ) ->
                    a // b |> Expression.Integer

                _ ->
                    Debug.todo ""

        "/" ->
            case ( simplifyExpression scope left |> Node.value, simplifyExpression scope right |> Node.value ) of
                ( Expression.Floatable a, Expression.Floatable b ) ->
                    a / b |> Expression.Floatable

                ( Expression.Integer a, Expression.Floatable b ) ->
                    toFloat a / b |> Expression.Floatable

                ( Expression.Floatable a, Expression.Integer b ) ->
                    a / toFloat b |> Expression.Floatable

                _ ->
                    Debug.todo ""

        "^" ->
            numberMath (^)
                (^)
                (simplifyExpression scope left |> Node.value)
                (simplifyExpression scope right |> Node.value)

        "++" ->
            case ( simplifyExpression scope left |> Node.value, simplifyExpression scope right |> Node.value ) of
                ( Expression.Literal a, Expression.Literal b ) ->
                    a ++ b |> Expression.Literal

                _ ->
                    Debug.todo ""

        "++" ->
            case ( simplifyExpression scope left |> Node.value, simplifyExpression scope right |> Node.value ) of
                ( Expression.ListExpr a, Expression.ListExpr b ) ->
                    a ++ b |> Expression.ListExpr

                _ ->
                    Debug.todo ""

        "::" ->
            case ( simplifyExpression scope left |> Node.value, simplifyExpression scope right |> Node.value ) of
                ( a, Expression.ListExpr b ) ->
                    Node Elm.Syntax.Range.emptyRange a :: b |> Expression.ListExpr

                _ ->
                    Debug.todo ""

        "==" ->
            Debug.todo "equals"

        "/=" ->
            Debug.todo "not equals"

        ">" ->
            comparisonCheck (>)
                (>)
                (simplifyExpression scope left |> Node.value)
                (simplifyExpression scope right |> Node.value)

        "<" ->
            comparisonCheck (<)
                (<)
                (simplifyExpression scope left |> Node.value)
                (simplifyExpression scope right |> Node.value)

        ">=" ->
            comparisonCheck (>=)
                (>=)
                (simplifyExpression scope left |> Node.value)
                (simplifyExpression scope right |> Node.value)

        "<=" ->
            comparisonCheck (<=)
                (<=)
                (simplifyExpression scope left |> Node.value)
                (simplifyExpression scope right |> Node.value)

        "||" ->
            case simplifyExpression scope left |> Node.value of
                Expression.FunctionOrValue _ "True" ->
                    Expression.FunctionOrValue [] "True"

                Expression.FunctionOrValue _ "False" ->
                    case simplifyExpression scope right |> Node.value of
                        Expression.FunctionOrValue _ "True" ->
                            Expression.FunctionOrValue [] "True"

                        Expression.FunctionOrValue _ "False" ->
                            Expression.FunctionOrValue [] "False"

                        _ ->
                            Debug.todo ""

                _ ->
                    Debug.todo ""

        "&&" ->
            case simplifyExpression scope left |> Node.value of
                Expression.FunctionOrValue _ "True" ->
                    case simplifyExpression scope right |> Node.value of
                        Expression.FunctionOrValue _ "True" ->
                            Expression.FunctionOrValue [] "True"

                        Expression.FunctionOrValue _ "False" ->
                            Expression.FunctionOrValue [] "False"

                        _ ->
                            Debug.todo ""

                Expression.FunctionOrValue _ "False" ->
                    Expression.FunctionOrValue [] "False"

                _ ->
                    Debug.todo ""

        _ ->
            Debug.todo ""


simplifyExpression : Scope -> Node Expression -> Node Expression
simplifyExpression scope (Node _ expression) =
    (case expression of
        Expression.UnitExpr ->
            expression

        Expression.Application nodes ->
            List.map (simplifyExpression scope) nodes |> Debug.todo ""

        Expression.OperatorApplication operator _ left right ->
            handleOperatorApplication scope expression operator left right

        Expression.FunctionOrValue moduleName name ->
            if String.left 1 name |> String.all Char.isUpper then
                expression

            else
                Debug.todo ""

        Expression.IfBlock condition ifTrue ifFalse ->
            case simplifyExpression scope condition |> Node.value of
                Expression.FunctionOrValue _ "True" ->
                    simplifyExpression scope ifTrue

                Expression.FunctionOrValue _ "False" ->
                    simplifyExpression scope ifFalse

                _ ->
                    Debug.todo ""

        Expression.PrefixOperator operator ->
            Expression.PrefixOperator operator

        Expression.Operator operator ->
            Expression.Operator operator

        Expression.Integer int ->
            Expression.Integer int

        Expression.Hex hex ->
            Expression.Integer hex

        Expression.Floatable float ->
            Expression.Floatable float

        Expression.Negation node ->
            case Node.value node of
                Expression.Integer int ->
                    Expression.Integer -int

                Expression.Hex hex ->
                    Expression.Integer -hex

                Expression.Floatable float ->
                    Expression.Floatable -float

                Expression.Negation node_ ->
                    simplifyExpression scope node_ |> Node.value

                _ ->
                    Debug.todo "Error"

        Expression.Literal string ->
            Expression.Literal string

        Expression.CharLiteral char ->
            Expression.CharLiteral char

        Expression.TupledExpression nodes ->
            nodes |> List.map (simplifyExpression scope) |> Expression.TupledExpression

        Expression.ParenthesizedExpression node_ ->
            simplifyExpression scope node_ |> Node.value

        Expression.LetExpression letBlock ->
            Debug.todo ""

        Expression.CaseExpression caseBlock ->
            Debug.todo ""

        Expression.LambdaExpression lambda ->
            Debug.todo ""

        Expression.RecordExpr nodes ->
            Debug.todo ""

        Expression.ListExpr nodes ->
            Debug.todo ""

        Expression.RecordAccess object field ->
            Debug.todo ""

        Expression.RecordAccessFunction string ->
            Debug.todo ""

        Expression.RecordUpdateExpression object nodes ->
            Debug.todo ""

        Expression.GLSLExpression glsl ->
            Debug.todo ""
    )
        |> Node Elm.Syntax.Range.emptyRange


expressionFitsPattern : Node Expression -> Node Pattern -> Bool
expressionFitsPattern (Node _ expression) ((Node _ pattern) as patternNode) =
    case expression of
        Expression.UnitExpr ->
            case pattern of
                Pattern.UnitPattern ->
                    True

                Pattern.AllPattern ->
                    True

                _ ->
                    False

        Expression.Application nodes ->
            Debug.todo ""

        Expression.OperatorApplication operator _ left right ->
            False

        Expression.FunctionOrValue _ valueName ->
            case pattern of
                Pattern.NamedPattern { name } [] ->
                    valueName == name

                Pattern.AllPattern ->
                    True

                _ ->
                    False

        Expression.IfBlock condition ifTrue ifFalse ->
            False

        Expression.PrefixOperator string ->
            Debug.todo ""

        Expression.Operator string ->
            False

        Expression.Integer int ->
            case pattern of
                Pattern.IntPattern int_ ->
                    int == int_

                Pattern.HexPattern int_ ->
                    int == int_

                Pattern.AllPattern ->
                    True

                _ ->
                    False

        Expression.Hex hex ->
            case pattern of
                Pattern.IntPattern int_ ->
                    hex == int_

                Pattern.HexPattern int_ ->
                    hex == int_

                Pattern.AllPattern ->
                    True

                _ ->
                    False

        Expression.Floatable float ->
            case pattern of
                Pattern.FloatPattern float_ ->
                    float == float_

                Pattern.AllPattern ->
                    True

                _ ->
                    False

        Expression.Negation node ->
            False

        Expression.Literal string ->
            case pattern of
                Pattern.StringPattern string_ ->
                    string == string_

                Pattern.AllPattern ->
                    True

                _ ->
                    False

        Expression.CharLiteral char ->
            case pattern of
                Pattern.CharPattern char_ ->
                    char == char_

                Pattern.AllPattern ->
                    True

                _ ->
                    False

        Expression.TupledExpression nodes ->
            List.all (\a -> expressionFitsPattern a patternNode) nodes

        Expression.ParenthesizedExpression node_ ->
            expressionFitsPattern node_ patternNode

        Expression.LetExpression letBlock ->
            False

        Expression.CaseExpression caseBlock ->
            False

        Expression.LambdaExpression lambda ->
            False

        Expression.RecordExpr nodes ->
            False

        Expression.ListExpr nodes ->
            Debug.todo ""

        Expression.RecordAccess object field ->
            False

        Expression.RecordAccessFunction string ->
            False

        Expression.RecordUpdateExpression object nodes ->
            False

        Expression.GLSLExpression glsl ->
            False


patternMatches : Zipper (Node Pattern) -> Node Expression -> Bool
patternMatches zipper node =
    List.Zipper.findFirst (expressionFitsPattern node) zipper
        |> (==) (Just zipper)


visitTree : Scope -> CallTree -> Reachability
visitTree scope tree =
    case tree of
        Unreachable_ { dependsOn } ->
            visitTree scope dependsOn

        CasePattern { dependsOn, caseOfDependsOn, patterns } ->
            case
                ( visitTree scope dependsOn
                , simplifyExpression scope caseOfDependsOn |> patternMatches patterns
                )
            of
                ( Reachable, True ) ->
                    Reachable

                ( Reachable, False ) ->
                    Unreachable

                ( reachable, _ ) ->
                    reachable

        CaseOf { dependsOn } ->
            Debug.todo "CaseOf tree"

        FunctionDeclaration_ { name } ->
            Reachable


visitFile : File -> List CallTree
visitFile file =
    file.declarations
        |> List.map
            (\(Node _ declaration) ->
                case declaration of
                    FunctionDeclaration function ->
                        let
                            declaration_ =
                                Node.value function.declaration
                        in
                        declaration_.expression |> visitExpression (Node.value declaration_.name)

                    _ ->
                        []
            )
        |> List.concat


visitExpression : String -> Node Expression -> List CallTree
visitExpression functionName node =
    visitExpressionHelper
        node
        |> List.map (\tree -> FunctionDeclaration_ { expr = node, name = functionName } |> tree)


wrapTree : (CallTree -> CallTree) -> List (CallTree -> CallTree) -> List (CallTree -> CallTree)
wrapTree wrapWith =
    List.map (\tree -> wrapWith >> tree)


visitExpressionHelper : Node Expression -> List (CallTree -> CallTree)
visitExpressionHelper node =
    case Node.value node of
        Expression.UnitExpr ->
            []

        Expression.Application nodes ->
            List.concatMap visitExpressionHelper nodes

        Expression.OperatorApplication _ _ left right ->
            visitExpressionHelper left
                ++ visitExpressionHelper right

        Expression.FunctionOrValue _ name ->
            if name == "unreachable" then
                [ \tree -> Unreachable_ { expr = node, dependsOn = tree } ]

            else
                []

        Expression.IfBlock condition ifTrue ifFalse ->
            visitExpressionHelper condition
                ++ visitExpressionHelper ifTrue
                ++ visitExpressionHelper ifFalse

        Expression.PrefixOperator _ ->
            []

        Expression.Operator _ ->
            []

        Expression.Integer _ ->
            []

        Expression.Hex _ ->
            []

        Expression.Floatable _ ->
            []

        Expression.Negation node_ ->
            visitExpressionHelper node_

        Expression.Literal _ ->
            []

        Expression.CharLiteral _ ->
            []

        Expression.TupledExpression nodes ->
            List.concatMap visitExpressionHelper nodes

        Expression.ParenthesizedExpression node_ ->
            visitExpressionHelper node_

        Expression.LetExpression letBlock ->
            List.concatMap
                (\(Node _ declaration) ->
                    case declaration of
                        LetFunction function ->
                            function.declaration |> Node.value |> .expression |> visitExpressionHelper

                        LetDestructuring _ letFunction ->
                            visitExpressionHelper letFunction
                )
                letBlock.declarations
                ++ visitExpressionHelper letBlock.expression

        Expression.CaseExpression caseBlock ->
            let
                patterns : List (Node Pattern)
                patterns =
                    caseBlock.cases |> List.map Tuple.first
            in
            visitExpressionHelper caseBlock.expression
                ++ List.concatMap
                    (\( pattern, expression ) ->
                        let
                            zipper =
                                List.Extra.findIndex ((==) pattern) patterns
                                    |> Maybe.withDefault 0
                                    |> (\splitIndex -> List.Extra.splitAt splitIndex patterns)
                                    |> (\( first, second ) -> List.Zipper.from first pattern (List.drop 1 second))

                            wrapWith tree =
                                CasePattern
                                    { expr = node
                                    , dependsOn = tree
                                    , caseOfDependsOn = caseBlock.expression
                                    , patterns = zipper
                                    }
                        in
                        visitExpressionHelper expression |> wrapTree wrapWith
                    )
                    caseBlock.cases

        Expression.LambdaExpression lambda ->
            visitExpressionHelper lambda.expression

        Expression.RecordExpr nodes ->
            List.concatMap (Node.value >> Tuple.second >> visitExpressionHelper) nodes

        Expression.ListExpr nodes ->
            List.concatMap visitExpressionHelper nodes

        Expression.RecordAccess node_ _ ->
            visitExpressionHelper node_

        Expression.RecordAccessFunction _ ->
            []

        Expression.RecordUpdateExpression _ nodes ->
            List.concatMap (Node.value >> Tuple.second >> visitExpressionHelper) nodes

        Expression.GLSLExpression _ ->
            []
