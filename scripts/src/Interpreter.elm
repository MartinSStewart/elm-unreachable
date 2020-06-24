module Interpreter exposing (CallTree(..), Reachability(..), visitFile, visitTree)

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression as Expression exposing (Expression, FunctionImplementation, LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range
import List.Extra as List


type CallTree
    = Unreachable_ { expr : Node Expression }
    | CasePattern
        { expr : Node Expression
        , caseOfDependsOn : Node Expression
        , children : List ( Node Pattern, Maybe CallTree )
        }
    | FunctionDeclaration_ { expr : Node Expression, name : String, child : CallTree }
    | OperatorApplication
        { expr : Node Expression
        , leftExpr : Node Expression -- We need to keep this in case there's short circuiting involved
        , leftChild : Maybe CallTree
        , rightChild : Maybe CallTree
        , operator : String -- We need to keep this to check if there's short circuiting involved
        }


type Reachability
    = Reachable
    | Unreachable
    | Unknown


type alias Scope =
    { topLevel : Dict String Expression.Function
    , parameters : Dict String Expression
    }


getBaseScope : File -> Scope
getBaseScope file =
    { topLevel =
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
    , parameters = Dict.empty
    }


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


topLevelDefinition :
    Scope
    -> ModuleName
    -> String
    -> Maybe { arguments : List (Node Pattern), expression : Node Expression }
topLevelDefinition scope moduleName functionName =
    case Dict.get functionName scope.parameters of
        Just paramValue ->
            Just { arguments = [], expression = Node Elm.Syntax.Range.emptyRange paramValue }

        Nothing ->
            Dict.get functionName scope.topLevel
                |> Maybe.map
                    (.declaration
                        >> Node.value
                        >> (\{ arguments, expression } -> { arguments = arguments, expression = expression })
                    )


scopeAddParameters : List ( Pattern, Expression ) -> Scope -> Scope
scopeAddParameters params { topLevel, parameters } =
    { topLevel = topLevel
    , parameters =
        params
            |> List.concatMap
                (\( param, expression ) ->
                    case param of
                        Pattern.CharPattern char ->
                            Debug.todo ""

                        Pattern.AllPattern ->
                            Debug.todo ""

                        Pattern.UnitPattern ->
                            Debug.todo ""

                        Pattern.StringPattern string ->
                            Debug.todo ""

                        Pattern.IntPattern int ->
                            Debug.todo ""

                        Pattern.HexPattern int ->
                            Debug.todo ""

                        Pattern.FloatPattern float ->
                            Debug.todo ""

                        Pattern.TuplePattern nodes ->
                            Debug.todo ""

                        Pattern.RecordPattern nodes ->
                            Debug.todo ""

                        Pattern.UnConsPattern node rest ->
                            Debug.todo ""

                        Pattern.ListPattern nodes ->
                            Debug.todo ""

                        Pattern.VarPattern var ->
                            [ ( var, expression ) ]

                        Pattern.NamedPattern qualifiedNameRef nodes ->
                            Debug.todo ""

                        Pattern.AsPattern node alias ->
                            Debug.todo ""

                        Pattern.ParenthesizedPattern node ->
                            Debug.todo ""
                )
            |> Dict.fromList
            |> Dict.union parameters
    }


simplifyExpression : Scope -> Node Expression -> Node Expression
simplifyExpression scope (Node _ expression) =
    (case expression of
        Expression.UnitExpr ->
            expression

        Expression.Application nodes ->
            let
                application : List (Node Expression)
                application =
                    List.map (simplifyExpression scope) nodes
            in
            case application of
                (Node _ (Expression.FunctionOrValue moduleName name)) :: rest ->
                    case topLevelDefinition scope moduleName name of
                        Just function ->
                            let
                                params : List ( Pattern, Expression )
                                params =
                                    List.zip
                                        (function.arguments |> List.map Node.value)
                                        (List.map Node.value rest)

                                paramCount =
                                    List.length params
                            in
                            if List.length rest == paramCount then
                                simplifyExpression (scopeAddParameters params scope) function.expression |> Node.value

                            else if List.length rest > paramCount then
                                simplifyExpression (scopeAddParameters params scope) function.expression
                                    :: List.drop paramCount rest
                                    |> Expression.Application
                                    |> Node Elm.Syntax.Range.emptyRange
                                    |> simplifyExpression scope
                                    |> Node.value

                            else
                                Expression.Application application

                        Nothing ->
                            Debug.todo ""

                _ ->
                    Debug.todo ""

        Expression.OperatorApplication operator _ left right ->
            handleOperatorApplication scope expression operator left right

        Expression.FunctionOrValue moduleName name ->
            if String.left 1 name |> String.all Char.isUpper then
                expression

            else
                case topLevelDefinition scope moduleName name of
                    Just function ->
                        if List.isEmpty function.arguments then
                            simplifyExpression scope function.expression |> Node.value

                        else
                            expression

                    Nothing ->
                        Debug.todo name

        Expression.IfBlock condition ifTrue ifFalse ->
            case simplifyExpression scope condition |> Node.value of
                Expression.FunctionOrValue _ "True" ->
                    simplifyExpression scope ifTrue |> Node.value

                Expression.FunctionOrValue _ "False" ->
                    simplifyExpression scope ifFalse |> Node.value

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


type PatternMatch
    = FoundMatch ( Node Pattern, Maybe CallTree )
    | FailedToSimplify


patternMatches : List ( Node Pattern, Maybe CallTree ) -> Node Expression -> PatternMatch
patternMatches patterns node =
    case List.find (Tuple.first >> expressionFitsPattern node) patterns of
        Just a ->
            FoundMatch a

        Nothing ->
            FailedToSimplify


visitTree : ( Scope, List CallTree ) -> List Reachability
visitTree ( scope, trees ) =
    List.map (\tree -> visitTreeHelper scope tree) trees


visitTreeHelper : Scope -> CallTree -> Reachability
visitTreeHelper scope tree =
    case tree of
        Unreachable_ _ ->
            Reachable

        CasePattern { children, caseOfDependsOn } ->
            case patternMatches children (simplifyExpression scope caseOfDependsOn) of
                FoundMatch ( _, Just child ) ->
                    visitTreeHelper scope child

                FoundMatch ( _, Nothing ) ->
                    Unreachable

                FailedToSimplify ->
                    Unknown

        FunctionDeclaration_ { name, child } ->
            visitTreeHelper scope child

        OperatorApplication { leftChild, rightChild, operator, leftExpr } ->
            Debug.todo ""


visitFile : File -> ( Scope, List CallTree )
visitFile file =
    ( getBaseScope file
    , file.declarations
        |> List.filterMap
            (\(Node _ declaration) ->
                case declaration of
                    FunctionDeclaration function ->
                        let
                            declaration_ =
                                Node.value function.declaration
                        in
                        declaration_.expression |> visitExpression (Node.value declaration_.name)

                    _ ->
                        Nothing
            )
    )


visitExpression : String -> Node Expression -> Maybe CallTree
visitExpression functionName node =
    visitExpressionHelper
        node
        |> Maybe.map
            (\child ->
                FunctionDeclaration_
                    { expr = node
                    , name = functionName
                    , child = child
                    }
            )


visitExpressionHelper : Node Expression -> Maybe CallTree
visitExpressionHelper node =
    case Node.value node of
        Expression.UnitExpr ->
            Nothing

        Expression.Application [ Node _ (Expression.FunctionOrValue _ "unreachable"), Node _ Expression.UnitExpr ] ->
            Unreachable_ { expr = node } |> Just

        Expression.Application _ ->
            Debug.todo ""

        --List.concatMap visitExpressionHelper nodes
        Expression.OperatorApplication operator _ left right ->
            OperatorApplication
                { expr = node
                , leftExpr = left
                , leftChild = visitExpressionHelper left
                , rightChild = visitExpressionHelper right
                , operator = operator
                }
                |> Just

        --visitExpressionHelper left
        --    ++ visitExpressionHelper right
        Expression.FunctionOrValue moduleName name ->
            Nothing

        Expression.IfBlock condition ifTrue ifFalse ->
            Debug.todo ""

        --visitExpressionHelper condition
        --    ++ visitExpressionHelper ifTrue
        --    ++ visitExpressionHelper ifFalse
        Expression.PrefixOperator _ ->
            Nothing

        Expression.Operator _ ->
            Nothing

        Expression.Integer _ ->
            Nothing

        Expression.Hex _ ->
            Nothing

        Expression.Floatable _ ->
            Nothing

        Expression.Negation node_ ->
            visitExpressionHelper node_

        Expression.Literal _ ->
            Nothing

        Expression.CharLiteral _ ->
            Nothing

        Expression.TupledExpression nodes ->
            Debug.todo ""

        --List.concatMap visitExpressionHelper nodes
        Expression.ParenthesizedExpression node_ ->
            visitExpressionHelper node_

        Expression.LetExpression letBlock ->
            Debug.todo ""

        --List.concatMap
        --    (\(Node _ declaration) ->
        --        case declaration of
        --            LetFunction function ->
        --                function.declaration |> Node.value |> .expression |> visitExpressionHelper
        --
        --            LetDestructuring _ letFunction ->
        --                visitExpressionHelper letFunction
        --    )
        --    letBlock.declarations
        --    ++ visitExpressionHelper letBlock.expression
        Expression.CaseExpression caseBlock ->
            let
                patterns : List ( Node Pattern, Maybe CallTree )
                patterns =
                    List.map
                        (\( pattern, expression ) ->
                            ( pattern, visitExpressionHelper expression )
                        )
                        caseBlock.cases
            in
            --visitExpressionHelper caseBlock.expression
            if List.any (Tuple.second >> (/=) Nothing) patterns then
                CasePattern
                    { expr = node
                    , caseOfDependsOn = caseBlock.expression
                    , children =
                        patterns
                    }
                    |> Just

            else
                Nothing

        Expression.LambdaExpression lambda ->
            visitExpressionHelper lambda.expression

        Expression.RecordExpr nodes ->
            Debug.todo ""

        --List.concatMap (Node.value >> Tuple.second >> visitExpressionHelper) nodes
        Expression.ListExpr nodes ->
            Debug.todo ""

        --List.concatMap visitExpressionHelper nodes
        Expression.RecordAccess node_ _ ->
            visitExpressionHelper node_

        Expression.RecordAccessFunction _ ->
            Nothing

        Expression.RecordUpdateExpression _ nodes ->
            Debug.todo ""

        --List.concatMap (Node.value >> Tuple.second >> visitExpressionHelper) nodes
        Expression.GLSLExpression _ ->
            Nothing
