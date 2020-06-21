module Interpreter exposing (CallTree(..), Reachability(..), visitFile, visitTree)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression as Expression exposing (Expression, LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range
import List.Extra
import List.Zipper exposing (Zipper)



--type Tree
--    = Operator { expr : Node Expression, dependsOnLeft : Tree, operator : String, dependsOnRight : Tree }
--    | Root { expr : Node Expression, dependsOn : Tree }
--    | CasePattern { expr : Node Expression, dependsOn : Tree, caseOfDependsOn : Tree, patterns : Zipper (Node Pattern) }
--    | CaseOf { expr : Node Expression, dependsOn : Tree }
--    | FunctionOrValue { expr : Node Expression, name : String }


type CallTree
    = Unreachable_ { expr : Node Expression, dependsOn : CallTree }
    | CasePattern { expr : Node Expression, dependsOn : CallTree, caseOfDependsOn : Node Expression, patterns : Zipper (Node Pattern) }
    | CaseOf { expr : Node Expression, dependsOn : CallTree }
    | FunctionDeclaration_ { expr : Node Expression, name : String }



--
--expressionToTree : Node Expression -> Tree
--expressionToTree expressionNode =
--    case Node.value expressionNode of
--        Expression.UnitExpr ->
--            Debug.todo "UnitExpr value"
--
--        Expression.Application nodes ->
--            Debug.todo "Application value"
--
--        Expression.OperatorApplication string infixDirection left right ->
--            Debug.todo "OperatorApplication value"
--
--        Expression.FunctionOrValue moduleName name ->
--            FunctionOrValue { expr = expressionNode, name = name }
--
--        Expression.IfBlock condition ifTrue ifFalse ->
--            Debug.todo "IfBlock value"
--
--        Expression.PrefixOperator string ->
--            Debug.todo "PrefixOperator value"
--
--        Expression.Operator string ->
--            Debug.todo "Operator value"
--
--        Expression.Integer int ->
--            Debug.todo "Integer value"
--
--        Expression.Hex int ->
--            Debug.todo "Hex value"
--
--        Expression.Floatable float ->
--            Debug.todo "Floatable value"
--
--        Expression.Negation node ->
--            Debug.todo "Negation value"
--
--        Expression.Literal string ->
--            Debug.todo "Literal value"
--
--        Expression.CharLiteral char ->
--            Debug.todo "CharLiteral value"
--
--        Expression.TupledExpression nodes ->
--            Debug.todo "TupledExpression value"
--
--        Expression.ParenthesizedExpression node ->
--            Debug.todo "ParenthesizedExpression value"
--
--        Expression.LetExpression letBlock ->
--            Debug.todo "LetExpression value"
--
--        Expression.CaseExpression caseBlock ->
--            Debug.todo "CaseExpression value"
--
--        Expression.LambdaExpression lambda ->
--            Debug.todo "LambdaExpression value"
--
--        Expression.RecordExpr nodes ->
--            Debug.todo "RecordExpr value"
--
--        Expression.ListExpr nodes ->
--            Debug.todo "ListExpr value"
--
--        Expression.RecordAccess object field ->
--            Debug.todo "RecordAccess value"
--
--        Expression.RecordAccessFunction string ->
--            Debug.todo "RecordAccessFunction value"
--
--        Expression.RecordUpdateExpression node nodes ->
--            Debug.todo "RecordUpdateExpression value"
--
--        Expression.GLSLExpression string ->
--            Debug.todo "GLSLExpression value"


type Reachability
    = Reachable
    | Unreachable
    | Unknown


simplifyExpression : Node Expression -> Node Expression
simplifyExpression (Node _ expression) =
    (case expression of
        Expression.UnitExpr ->
            expression

        Expression.Application nodes ->
            Debug.todo ""

        Expression.OperatorApplication operator _ left right ->
            case ( operator, simplifyExpression left |> Node.value, simplifyExpression right |> Node.value ) of
                ( "+", Expression.Integer a, Expression.Integer b ) ->
                    a + b |> Expression.Integer

                ( "+", Expression.Floatable a, Expression.Floatable b ) ->
                    a + b |> Expression.Floatable

                ( "+", Expression.Integer a, Expression.Floatable b ) ->
                    toFloat a + b |> Expression.Floatable

                ( "+", Expression.Floatable a, Expression.Integer b ) ->
                    a + toFloat b |> Expression.Floatable

                ( "-", Expression.Integer a, Expression.Integer b ) ->
                    a - b |> Expression.Integer

                ( "-", Expression.Floatable a, Expression.Floatable b ) ->
                    a - b |> Expression.Floatable

                ( "-", Expression.Integer a, Expression.Floatable b ) ->
                    toFloat a - b |> Expression.Floatable

                ( "-", Expression.Floatable a, Expression.Integer b ) ->
                    a - toFloat b |> Expression.Floatable

                ( "*", Expression.Integer a, Expression.Integer b ) ->
                    a * b |> Expression.Integer

                ( "*", Expression.Floatable a, Expression.Floatable b ) ->
                    a * b |> Expression.Floatable

                ( "*", Expression.Integer a, Expression.Floatable b ) ->
                    toFloat a * b |> Expression.Floatable

                ( "*", Expression.Floatable a, Expression.Integer b ) ->
                    a * toFloat b |> Expression.Floatable

                ( "//", Expression.Integer a, Expression.Integer b ) ->
                    a // b |> Expression.Integer

                ( "/", Expression.Floatable a, Expression.Floatable b ) ->
                    a / b |> Expression.Floatable

                ( "^", Expression.Integer a, Expression.Integer b ) ->
                    a ^ b |> Expression.Integer

                ( "^", Expression.Floatable a, Expression.Floatable b ) ->
                    a ^ b |> Expression.Floatable

                ( "^", Expression.Integer a, Expression.Floatable b ) ->
                    toFloat a ^ b |> Expression.Floatable

                ( "^", Expression.Floatable a, Expression.Integer b ) ->
                    a ^ toFloat b |> Expression.Floatable

                _ ->
                    Debug.todo ""

        Expression.FunctionOrValue moduleName name ->
            if String.left 1 name |> String.all Char.isUpper then
                expression

            else
                Debug.todo ""

        Expression.IfBlock condition ifTrue ifFalse ->
            Debug.todo ""

        Expression.PrefixOperator string ->
            Debug.todo ""

        Expression.Operator string ->
            Debug.todo ""

        Expression.Integer int ->
            expression

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
                    simplifyExpression node_ |> Node.value

                _ ->
                    Debug.todo "Error"

        Expression.Literal string ->
            Expression.Literal string

        Expression.CharLiteral char ->
            Expression.CharLiteral char

        Expression.TupledExpression nodes ->
            nodes |> List.map simplifyExpression |> Expression.TupledExpression

        Expression.ParenthesizedExpression node_ ->
            simplifyExpression node_ |> Node.value

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


visitTree : CallTree -> Reachability
visitTree tree =
    case tree of
        Unreachable_ { dependsOn } ->
            visitTree dependsOn

        CasePattern { dependsOn, caseOfDependsOn, patterns } ->
            case
                ( visitTree dependsOn
                , simplifyExpression caseOfDependsOn |> patternMatches patterns
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
