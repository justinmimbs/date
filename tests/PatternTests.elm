module PatternTests exposing (test_fromString)

import Pattern exposing (Pattern, Token)
import Shim exposing (Expectation, Test, describe, equal, test)


f : Char -> Int -> Token
f =
    Pattern.Field


l : String -> Token
l =
    Pattern.Literal


test_fromString : Test
test_fromString =
    let
        toTest : ( String, Pattern ) -> Test
        toTest ( input, expected ) =
            test input <| \() -> Pattern.fromString input |> equal expected
    in
    describe "fromString" <|
        List.map
            toTest
            [ ( "aaa", [ f 'a' 3 ] )
            , ( "abbccc", [ f 'a' 1, f 'b' 2, f 'c' 3 ] )
            , ( "''dddd''eeeee", [ l "'", f 'd' 4, l "'", f 'e' 5 ] )
            , ( "aa-bb-cc//#!0.dd", [ f 'a' 2, l "-", f 'b' 2, l "-", f 'c' 2, l "//#!0.", f 'd' 2 ] )
            , ( "a'''bbb'", [ f 'a' 1, l "'bbb" ] )
            , ( "a'''bbb", [ f 'a' 1, l "'bbb" ] )
            , ( "'o''clock'", [ l "o'clock" ] )
            , ( "'''aaa ' '' - ''' '' '' '..' a '", [ l "'aaa  ' - ' ' ' .. a " ] )
            ]
