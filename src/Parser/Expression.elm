module Parser.Expression
    exposing
        ( Assoc(..)
        , Operator(..)
        , OperatorTable
        , buildExpressionParser
        )

{-| Tools for building parsers for prefix, postfix or infix operator expressions.


# Builder

@docs buildExpressionParser


# Types

@docs Operator, OperatorTable, Assoc

-}

import Parser exposing (..)
import Tuple


{-| Data Type representing an operator.
An operator can either be binary infix and require an associativity,
or it can be unary prefix or postfix.
-}
type Operator a
    = Infix (Parser (a -> a -> a)) Assoc
    | Prefix (Parser (a -> a))
    | Postfix (Parser (a -> a))


{-| This is just a List of Lists of Operators.
The first inner list has the highest precedence and the last has the lowest.
If two operators are on the same inner list, they have the same precedence.
-}
type alias OperatorTable a =
    List (List (Operator a))


{-| Data type representing the associativity of an operator.
-}
type Assoc
    = AssocNone
    | AssocLeft
    | AssocRight


{-| Build an expression parser for terms from a table of operators,
taking into account precedence and associativity.

The following would define a simple arithmetic parser.

    unaryOp : (a -> a) -> Parser () -> Parser (a -> a)
    unaryOp f p =
        succeed f
            |. p
            |. spaces

    binaryOp : (a -> a -> a) -> Parser () -> Parser (a -> a -> a)
    binaryOp f p =
        succeed f
            |. p
            |. spaces

    operators : OperatorTable number
    operators =
        [ [ Prefix (unaryOp negate (symbol "-")), Prefix (unaryOp identity (symbol "+")) ]
        , [ Postfix (unaryOp (\x -> x + 1) (symbol "++")) ]
        , [ Infix (binaryOp (*) (symbol "*")) AssocLeft ]
        , [ Infix (binaryOp (+) (symbol "+")) AssocLeft, Infix (binaryOp (-) (symbol "-")) AssocLeft ]
        ]

    term : Parser Int
    term =
        oneOf
            [ parens <| lazy (_ -> expr)
            , int |. spaces
            ]

    expr : Parser Int
    expr =
        buildExpressionParser operators (lazy <| _ -> term)

-}
buildExpressionParser : OperatorTable a -> Parser a -> Parser a
buildExpressionParser operators simpleExpr =
    List.foldl makeParser simpleExpr operators



-- HELPERS


makeParser : List (Operator a) -> Parser a -> Parser a
makeParser ops term =
    let
        { rassoc, lassoc, nassoc, prefix, postfix } =
            List.foldr splitOp initOps ops

        rassocOp : Parser (a -> a -> a)
        rassocOp =
            oneOf rassoc

        lassocOp : Parser (a -> a -> a)
        lassocOp =
            oneOf lassoc

        nassocOp : Parser (a -> a -> a)
        nassocOp =
            oneOf nassoc

        prefixOp : Parser (a -> a)
        prefixOp =
            oneOf prefix

        postfixOp : Parser (a -> a)
        postfixOp =
            oneOf postfix

        ambiguous : String -> Parser (a -> a -> a) -> Parser a
        ambiguous assoc op =
            backtrackable
                (op
                    |> andThen (\_ -> problem ("ambiguous use of a " ++ assoc ++ " associative operator"))
                )

        ambiguousRight : Parser a
        ambiguousRight =
            ambiguous "right" rassocOp

        ambiguousLeft : Parser a
        ambiguousLeft =
            ambiguous "left" lassocOp

        ambiguousNon : Parser a
        ambiguousNon =
            ambiguous "non" nassocOp

        termP : Parser a
        termP =
            succeed (\pre x post -> post (pre x))
                |= prefixP
                |= term
                |= postfixP

        prefixP : Parser (a -> a)
        prefixP =
            oneOf
                [ prefixOp
                , succeed identity
                ]

        postfixP : Parser (a -> a)
        postfixP =
            oneOf
                [ postfixOp
                , succeed identity
                ]

        rassocP : a -> Parser a
        rassocP x =
            oneOf
                [ succeed (\f y -> f x y)
                    |= rassocOp
                    |= (termP |> andThen rassocP1)
                , ambiguousLeft
                , ambiguousNon
                ]

        rassocP1 : a -> Parser a
        rassocP1 x =
            oneOf
                [ rassocP x
                , succeed x
                ]

        lassocP : a -> Parser a
        lassocP x =
            oneOf
                [ succeed Tuple.pair
                    |= lassocOp
                    |= termP
                    |> andThen (\( f, y ) -> lassocP1 (f x y))
                , ambiguousRight
                , ambiguousNon
                ]

        lassocP1 : a -> Parser a
        lassocP1 x =
            oneOf
                [ lassocP x
                , succeed x
                ]

        nassocP : a -> Parser a
        nassocP x =
            succeed Tuple.pair
                |= nassocOp
                |= termP
                |> andThen (\( f, y ) -> oneOf [ ambiguousRight, ambiguousLeft, ambiguousNon, succeed (f x y) ])
    in
        termP
            |> andThen (\x -> oneOf [ rassocP x, lassocP x, nassocP x, succeed x ])


type alias Ops a =
    { rassoc : List (Parser (a -> a -> a))
    , lassoc : List (Parser (a -> a -> a))
    , nassoc : List (Parser (a -> a -> a))
    , prefix : List (Parser (a -> a))
    , postfix : List (Parser (a -> a))
    }


initOps =
    { rassoc = [], lassoc = [], nassoc = [], prefix = [], postfix = [] }


splitOp : Operator a -> Ops a -> Ops a
splitOp operator ({ rassoc, lassoc, nassoc, prefix, postfix } as ops) =
    case operator of
        Infix op assoc ->
            case assoc of
                AssocNone ->
                    { ops | nassoc = op :: ops.nassoc }

                AssocLeft ->
                    { ops | lassoc = op :: ops.lassoc }

                AssocRight ->
                    { ops | rassoc = op :: ops.rassoc }

        Prefix op ->
            { ops | prefix = op :: ops.prefix }

        Postfix op ->
            { ops | postfix = op :: ops.postfix }
