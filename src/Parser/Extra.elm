module Parser.Extra
    exposing
        ( between
        , braces
        , brackets
        , by
        , many
        , parens
        , some
        , spacy
        )

{-| Some convenience parser combinators.


# Combinators

@docs many, some, by, spacy, between, parens, braces, brackets

-}

import Parser exposing (..)


{-| Apply a parser one or more times and return a list of the results.
-}
some : Parser a -> Parser (List a)
some item =
    succeed (::)
        |= item
        |= many item


{-| Apply a parser zero or more times and return a list of the results.
-}
many : Parser a -> Parser (List a)
many item =
    let
        helper vs =
            oneOf
                [ succeed (\v -> Loop (v :: vs))
                    |= item
                , succeed ()
                    |> map (\() -> Done (List.reverse vs))
                ]
    in
    loop [] helper


{-| Parse an item one or more times separated by a separator.
-}
by : Parser () -> Parser a -> Parser (List a)
by sep item =
    let
        more =
            succeed identity
                |. backtrackable sep
                |= item
    in
    succeed (::)
        |= item
        |= many more


{-| Parse an item with spaces before and after.
-}
spacy : Parser () -> Parser ()
spacy item =
    succeed identity
        |. spaces
        |= item
        |. spaces


{-| Parse an item between two other parsers.
-}
between : Parser opening -> Parser closing -> Parser a -> Parser a
between opening closing item =
    succeed identity
        |. opening
        |= item
        |. closing


{-| Parse an expression between parenthesis.

    parens item == between (symbol "(") (symbol ")") item

-}
parens : Parser a -> Parser a
parens =
    between (symbol "(") (symbol ")")


{-| Parse an expression between curly braces.

    braces item == between (symbol "{") (symbol "}") item

-}
braces : Parser a -> Parser a
braces =
    between (symbol "{") (symbol "}")


{-| Parse an expression between square brackets.

    brackets item == between (symbol "[") (symbol "]") item

-}
brackets : Parser a -> Parser a
brackets =
    between (symbol "[") (symbol "]")
