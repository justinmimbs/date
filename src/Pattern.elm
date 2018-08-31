module Pattern exposing (Pattern, Token(..), fromString)

import Char
import Parser exposing ((|.), (|=), Parser)



-- date formatting pattern


type alias Pattern =
    List Token


type Token
    = Field Char Int
    | Literal String


fromString : String -> Pattern
fromString str =
    Parser.run (patternHelp []) str
        |> Result.withDefault [ Literal str ]



-- parser


field : Parser Token
field =
    Parser.chompIf Char.isAlpha
        |> Parser.getChompedString
        |> Parser.andThen fieldRepeats


fieldRepeats : String -> Parser Token
fieldRepeats str =
    case String.toList str of
        [ char ] ->
            Parser.succeed (\x y -> Field char (1 + (y - x)))
                |= Parser.getOffset
                |. Parser.chompWhile ((==) char)
                |= Parser.getOffset

        _ ->
            Parser.problem "expected exactly one char"


escapedQuote : Parser Token
escapedQuote =
    Parser.succeed (Literal "'")
        |. Parser.token "''"


literal : Parser Token
literal =
    Parser.succeed ()
        |. Parser.chompIf isLiteralChar
        |. Parser.chompWhile isLiteralChar
        |> Parser.getChompedString
        |> Parser.map Literal


isLiteralChar : Char -> Bool
isLiteralChar char =
    char /= '\'' && not (Char.isAlpha char)


quoted : Parser Token
quoted =
    Parser.succeed Literal
        |. Parser.chompIf ((==) '\'')
        |= quotedHelp ""
        |. Parser.oneOf
            [ Parser.chompIf ((==) '\'')
            , Parser.end -- lenient parse for unclosed quotes
            ]


quotedHelp : String -> Parser String
quotedHelp result =
    Parser.oneOf
        [ Parser.succeed ()
            |. Parser.chompIf ((/=) '\'')
            |. Parser.chompWhile ((/=) '\'')
            |> Parser.getChompedString
            |> Parser.andThen (\str -> quotedHelp (result ++ str))
        , Parser.token "''"
            |> Parser.andThen (\_ -> quotedHelp (result ++ "'"))
        , Parser.succeed result
        ]


patternHelp : List Token -> Parser (List Token)
patternHelp tokens =
    Parser.oneOf
        [ Parser.oneOf
            [ field
            , literal
            , escapedQuote
            , quoted
            ]
            |> Parser.andThen (\token -> patternHelp (token :: tokens))
        , Parser.lazy
            (\_ -> Parser.succeed (finalize tokens))
        ]


{-| Reverse list and combine consecutive Literals.
-}
finalize : List Token -> List Token
finalize =
    List.foldl
        (\token tokens ->
            case ( token, tokens ) of
                ( Literal x, (Literal y) :: rest ) ->
                    Literal (x ++ y) :: rest

                _ ->
                    token :: tokens
        )
        []
