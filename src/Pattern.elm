module Pattern exposing (..)

import Char
import Parser exposing ((|.), (|=), Parser)


ex : List (Result (List Parser.DeadEnd) (List Field))
ex =
    [ "aaa"
    , "abbccc"
    ]
        |> List.map (Parser.run parsePattern)



-------------------------------------------------------------------------------


type Field
    = Field Char Int


parseField : Parser Field
parseField =
    Parser.chompIf Char.isAlpha
        |> Parser.getChompedString
        |> Parser.andThen parseFieldRepeats


parseFieldRepeats : String -> Parser Field
parseFieldRepeats str =
    case String.toList str of
        [ char ] ->
            Parser.succeed (\x y -> Field char (1 + (y - x)))
                |= Parser.getOffset
                |. Parser.chompWhile ((==) char)
                |= Parser.getOffset

        _ ->
            Parser.problem "expected one char"


parsePattern : Parser (List Field)
parsePattern =
    parsePatternHelp []


parsePatternHelp : List Field -> Parser (List Field)
parsePatternHelp fields =
    Parser.lazy <|
        \_ ->
            Parser.oneOf
                [ parseField
                    |> Parser.andThen (\field -> parsePatternHelp (field :: fields))
                , Parser.succeed (List.reverse fields)
                ]
