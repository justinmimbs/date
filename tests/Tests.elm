module Tests exposing (suite)

import Date exposing (Date, Interval(..), Unit(..))
import Language
import Shim exposing (Expectation, Test, describe, equal, test)
import Time exposing (Month(..), Weekday(..))



-- import Expect exposing (Expectation)
-- import Test exposing (Test, describe, test)
-------------------------------------------------------------------------------


{-| temporary collection of all tests

    run suite

-}
suite : Test
suite =
    describe "Date"
        [ test_CalendarDate
        , test_RataDie
        , test_WeekDate
        , test_format
        , test_formatWithLanguage
        , test_add
        , test_diff
        , test_floor
        , test_ceiling
        , test_range
        , test_fromIsoString
        , test_fromOrdinalDate
        , test_fromCalendarDate
        , test_fromWeekDate
        , test_numberToMonth
        , test_numberToWeekday
        , test_compare
        , test_isBetween
        , test_min
        , test_max
        , test_clamp
        ]



-------------------------------------------------------------------------------


test_CalendarDate : Test
test_CalendarDate =
    describe "CalendarDate"
        [ describe "CalendarDate and Date are are isomorphic"
            (List.concat
                [ List.range 1897 1905
                , List.range 1997 2025
                , List.range -5 5
                , List.range -105 -95
                , List.range -405 -395
                ]
                |> List.concatMap calendarDatesInYear
                |> List.map
                    (\calendarDate ->
                        test (Debug.toString calendarDate) <|
                            \() -> expectIsomorphism fromCalendarDate toCalendarDate calendarDate
                    )
            )
        ]


test_RataDie : Test
test_RataDie =
    describe "RataDie"
        [ test "a list of contiguous CalendarDates, converted to RataDie, is equivalent to a list of contiguous integers" <|
            \() ->
                List.range 1997 2025
                    |> List.concatMap (calendarDatesInYear >> List.map (fromCalendarDate >> Date.toRataDie))
                    |> equal
                        (List.range
                            (Date.fromCalendarDate 1997 Jan 1 |> Date.toRataDie)
                            (Date.fromCalendarDate 2025 Dec 31 |> Date.toRataDie)
                        )
        ]


test_WeekDate : Test
test_WeekDate =
    describe "WeekDate"
        [ describe "WeekDate and Date are isomorphic"
            (List.concat
                [ List.range 1997 2025
                , List.range -5 5
                ]
                |> List.concatMap calendarDatesInYear
                |> List.map
                    (\calendarDate ->
                        test (Debug.toString calendarDate) <|
                            \() -> expectIsomorphism toWeekDate fromWeekDate (fromCalendarDate calendarDate)
                    )
            )
        , describe "toWeekDate produces results that match samples"
            ([ ( CalendarDate 2005 Jan 1, WeekDate 2004 53 Sat )
             , ( CalendarDate 2005 Jan 2, WeekDate 2004 53 Sun )
             , ( CalendarDate 2005 Dec 31, WeekDate 2005 52 Sat )
             , ( CalendarDate 2007 Jan 1, WeekDate 2007 1 Mon )
             , ( CalendarDate 2007 Dec 30, WeekDate 2007 52 Sun )
             , ( CalendarDate 2007 Dec 31, WeekDate 2008 1 Mon )
             , ( CalendarDate 2008 Jan 1, WeekDate 2008 1 Tue )
             , ( CalendarDate 2008 Dec 28, WeekDate 2008 52 Sun )
             , ( CalendarDate 2008 Dec 29, WeekDate 2009 1 Mon )
             , ( CalendarDate 2008 Dec 30, WeekDate 2009 1 Tue )
             , ( CalendarDate 2008 Dec 31, WeekDate 2009 1 Wed )
             , ( CalendarDate 2009 Jan 1, WeekDate 2009 1 Thu )
             , ( CalendarDate 2009 Dec 31, WeekDate 2009 53 Thu )
             , ( CalendarDate 2010 Jan 1, WeekDate 2009 53 Fri )
             , ( CalendarDate 2010 Jan 2, WeekDate 2009 53 Sat )
             , ( CalendarDate 2010 Jan 3, WeekDate 2009 53 Sun )
             ]
                |> List.map
                    (\( calendarDate, weekDate ) ->
                        test (Debug.toString calendarDate) <|
                            \() -> fromCalendarDate calendarDate |> toWeekDate |> equal weekDate
                    )
            )
        ]


test_format : Test
test_format =
    let
        toTest : Date -> ( String, String ) -> Test
        toTest date ( pattern, expected ) =
            test ("\"" ++ pattern ++ "\" " ++ Debug.toString date) <|
                \() -> date |> Date.format pattern |> equal expected
    in
    describe "format"
        [ describe "replaces supported character patterns" <|
            List.map
                (toTest (Date.fromCalendarDate 2001 Jan 2))
                [ ( "y", "2001" )
                , ( "yy", "01" )
                , ( "yyy", "2001" )
                , ( "yyyy", "2001" )
                , ( "yyyyy", "02001" )
                , ( "Y", "2001" )
                , ( "YY", "01" )
                , ( "YYY", "2001" )
                , ( "YYYY", "2001" )
                , ( "YYYYY", "02001" )
                , ( "Q", "1" )
                , ( "QQ", "1" )
                , ( "QQQ", "Q1" )
                , ( "QQQQ", "1st" )
                , ( "QQQQQ", "1" )
                , ( "QQQQQQ", "" )
                , ( "M", "1" )
                , ( "MM", "01" )
                , ( "MMM", "Jan" )
                , ( "MMMM", "January" )
                , ( "MMMMM", "J" )
                , ( "MMMMMM", "" )
                , ( "w", "1" )
                , ( "ww", "01" )
                , ( "www", "" )
                , ( "d", "2" )
                , ( "dd", "02" )
                , ( "ddd", "2nd" )
                , ( "dddd", "" )
                , ( "D", "2" )
                , ( "DD", "02" )
                , ( "DDD", "002" )
                , ( "DDDD", "" )
                , ( "E", "Tue" )
                , ( "EE", "Tue" )
                , ( "EEE", "Tue" )
                , ( "EEEE", "Tuesday" )
                , ( "EEEEE", "T" )
                , ( "EEEEEE", "Tu" )
                , ( "EEEEEEE", "" )
                , ( "e", "2" )
                , ( "ee", "2" )
                , ( "eee", "Tue" )
                , ( "eeee", "Tuesday" )
                , ( "eeeee", "T" )
                , ( "eeeeee", "Tu" )
                , ( "eeeeeee", "" )
                ]
        , describe "removes unsupported pattern characters" <|
            List.map
                (toTest (Date.fromCalendarDate 2008 Dec 31))
                [ ( "ABCFGHIJKLNOPRSTUVWXZabcfghijklmnopqrstuvxz", "" )
                ]
        , describe "ignores non-alpha characters" <|
            List.map
                (toTest (Date.fromCalendarDate 2008 Dec 31))
                [ ( "0123456789 .,\\//:-%", "0123456789 .,\\//:-%" )
                ]
        , describe "handles escaped characters and escaped escape characters" <|
            List.map
                (toTest (Date.fromCalendarDate 2001 Jan 2))
                [ ( "'yYQMwdDEe'", "yYQMwdDEe" )
                , ( "''' '' ''' ''", "' ' ' '" )
                , ( "'yyyy:' yyyy", "yyyy: 2001" )
                ]
        , describe "is lenient on unclosed quotes" <|
            List.map
                (toTest (Date.fromCalendarDate 2001 Jan 2))
                [ ( "yyyy 'yyyy", "2001 yyyy" )
                ]
        , describe "formats day ordinals" <|
            List.map
                (\( n, string ) ->
                    toTest (Date.fromCalendarDate 2001 Jan n) ( "ddd", string )
                )
                [ ( 1, "1st" )
                , ( 2, "2nd" )
                , ( 3, "3rd" )
                , ( 4, "4th" )
                , ( 5, "5th" )
                , ( 6, "6th" )
                , ( 7, "7th" )
                , ( 8, "8th" )
                , ( 9, "9th" )
                , ( 10, "10th" )
                , ( 11, "11th" )
                , ( 12, "12th" )
                , ( 13, "13th" )
                , ( 14, "14th" )
                , ( 15, "15th" )
                , ( 16, "16th" )
                , ( 17, "17th" )
                , ( 18, "18th" )
                , ( 19, "19th" )
                , ( 20, "20th" )
                , ( 21, "21st" )
                , ( 22, "22nd" )
                , ( 23, "23rd" )
                , ( 24, "24th" )
                , ( 25, "25th" )
                , ( 26, "26th" )
                , ( 27, "27th" )
                , ( 28, "28th" )
                , ( 29, "29th" )
                , ( 30, "30th" )
                , ( 31, "31st" )
                ]
        , describe "formats with sample patterns as expected" <|
            List.map
                (toTest (Date.fromCalendarDate 2008 Dec 31))
                [ ( "yyyy-MM-dd", "2008-12-31" )
                , ( "yyyy-DDD", "2008-366" )
                , ( "YYYY-'W'ww-e", "2009-W01-3" )
                , ( "M/d/y", "12/31/2008" )
                , ( "''yy", "'08" )
                ]
        ]


test_formatWithLanguage : Test
test_formatWithLanguage =
    let
        toTest : Date -> ( String, String ) -> Test
        toTest date ( pattern, expected ) =
            test ("\"" ++ pattern ++ "\" " ++ Debug.toString date) <|
                \() -> date |> Date.formatWithLanguage Language.fr pattern |> equal expected
    in
    describe "formatWithLanguage"
        [ describe "replaces names as expected" <|
            List.map
                (toTest (Date.fromCalendarDate 2001 Jan 1))
                [ ( "MMM", "janv." )
                , ( "MMMM", "janvier" )
                , ( "MMMMM", "j" )
                , ( "MMMMMM", "" )
                , ( "d", "1" )
                , ( "dd", "01" )
                , ( "ddd", "1er" )
                , ( "dddd", "" )
                , ( "E", "lun" )
                , ( "EE", "lun" )
                , ( "EEE", "lun" )
                , ( "EEEE", "lundi" )
                , ( "EEEEE", "l" )
                , ( "EEEEEE", "lu" )
                , ( "EEEEEEE", "" )
                ]
        ]


test_add : Test
test_add =
    let
        toTest : ( Int, Month, Int ) -> Int -> Unit -> ( Int, Month, Int ) -> Test
        toTest ( y1, m1, d1 ) n unit (( y2, m2, d2 ) as expected) =
            test (Debug.toString ( y1, m1, d1 ) ++ " + " ++ Debug.toString n ++ " " ++ Debug.toString unit ++ " => " ++ Debug.toString expected) <|
                \() ->
                    Date.fromCalendarDate y1 m1 d1 |> Date.add unit n |> equal (Date.fromCalendarDate y2 m2 d2)
    in
    describe "add"
        [ describe "add 0 x == x" <|
            List.map
                (\unit -> toTest ( 2000, Jan, 1 ) 0 unit ( 2000, Jan, 1 ))
                [ Years, Months, Weeks, Days ]
        , describe "adding positive numbers works as expected"
            [ toTest ( 2000, Jan, 1 ) 2 Years ( 2002, Jan, 1 )
            , toTest ( 2000, Jan, 1 ) 2 Months ( 2000, Mar, 1 )
            , toTest ( 2000, Jan, 1 ) 2 Weeks ( 2000, Jan, 15 )
            , toTest ( 2000, Jan, 1 ) 2 Days ( 2000, Jan, 3 )
            , toTest ( 2000, Jan, 1 ) 18 Years ( 2018, Jan, 1 )
            , toTest ( 2000, Jan, 1 ) 18 Months ( 2001, Jul, 1 )
            , toTest ( 2000, Jan, 1 ) 18 Weeks ( 2000, May, 6 )
            , toTest ( 2000, Jan, 1 ) 36 Days ( 2000, Feb, 6 )
            ]
        , describe "adding negative numbers works as expected"
            [ toTest ( 2000, Jan, 1 ) -2 Years ( 1998, Jan, 1 )
            , toTest ( 2000, Jan, 1 ) -2 Months ( 1999, Nov, 1 )
            , toTest ( 2000, Jan, 1 ) -2 Weeks ( 1999, Dec, 18 )
            , toTest ( 2000, Jan, 1 ) -2 Days ( 1999, Dec, 30 )
            , toTest ( 2000, Jan, 1 ) -18 Years ( 1982, Jan, 1 )
            , toTest ( 2000, Jan, 1 ) -18 Months ( 1998, Jul, 1 )
            , toTest ( 2000, Jan, 1 ) -18 Weeks ( 1999, Aug, 28 )
            , toTest ( 2000, Jan, 1 ) -18 Days ( 1999, Dec, 14 )
            ]
        , describe "adding Years from a leap day clamps overflow to the end of February"
            [ toTest ( 2000, Feb, 29 ) 1 Years ( 2001, Feb, 28 )
            , toTest ( 2000, Feb, 29 ) 4 Years ( 2004, Feb, 29 )
            ]
        , describe "adding Months clamps overflow to the end of a short month"
            [ toTest ( 2000, Jan, 31 ) 1 Months ( 2000, Feb, 29 )
            , toTest ( 2000, Jan, 31 ) 2 Months ( 2000, Mar, 31 )
            , toTest ( 2000, Jan, 31 ) 3 Months ( 2000, Apr, 30 )
            , toTest ( 2000, Jan, 31 ) 13 Months ( 2001, Feb, 28 )
            ]
        ]


test_diff : Test
test_diff =
    let
        toTest : ( Int, Month, Int ) -> ( Int, Month, Int ) -> Int -> Unit -> Test
        toTest ( y1, m1, d1 ) ( y2, m2, d2 ) expected unit =
            test (Debug.toString ( y2, m2, d2 ) ++ " - " ++ Debug.toString ( y1, m1, d1 ) ++ " => " ++ Debug.toString expected ++ " " ++ Debug.toString unit) <|
                \() ->
                    Date.diff unit (Date.fromCalendarDate y1 m1 d1) (Date.fromCalendarDate y2 m2 d2) |> equal expected
    in
    describe "diff"
        [ describe "diff x x == 0" <|
            List.map
                (\unit -> toTest ( 2000, Jan, 1 ) ( 2000, Jan, 1 ) 0 unit)
                [ Years, Months, Weeks, Days ]
        , describe "diff x y == -(diff y x)" <|
            let
                ( x, y ) =
                    ( Date.fromCalendarDate 2000 Jan 1, Date.fromCalendarDate 2017 Sep 28 )
            in
            List.map
                (\unit -> test (Debug.toString unit) <| \() -> Date.diff unit x y |> equal (negate (Date.diff unit y x)))
                [ Years, Months, Weeks, Days ]
        , describe "`diff earlier later` results in positive numbers"
            [ toTest ( 2000, Jan, 1 ) ( 2002, Jan, 1 ) 2 Years
            , toTest ( 2000, Jan, 1 ) ( 2000, Mar, 1 ) 2 Months
            , toTest ( 2000, Jan, 1 ) ( 2000, Jan, 15 ) 2 Weeks
            , toTest ( 2000, Jan, 1 ) ( 2000, Jan, 3 ) 2 Days
            , toTest ( 2000, Jan, 1 ) ( 2018, Jan, 1 ) 18 Years
            , toTest ( 2000, Jan, 1 ) ( 2001, Jul, 1 ) 18 Months
            , toTest ( 2000, Jan, 1 ) ( 2000, May, 6 ) 18 Weeks
            , toTest ( 2000, Jan, 1 ) ( 2000, Feb, 6 ) 36 Days
            ]
        , describe "`diff later earlier` results in negative numbers"
            [ toTest ( 2000, Jan, 1 ) ( 1998, Jan, 1 ) -2 Years
            , toTest ( 2000, Jan, 1 ) ( 1999, Nov, 1 ) -2 Months
            , toTest ( 2000, Jan, 1 ) ( 1999, Dec, 18 ) -2 Weeks
            , toTest ( 2000, Jan, 1 ) ( 1999, Dec, 30 ) -2 Days
            , toTest ( 2000, Jan, 1 ) ( 1982, Jan, 1 ) -18 Years
            , toTest ( 2000, Jan, 1 ) ( 1998, Jul, 1 ) -18 Months
            , toTest ( 2000, Jan, 1 ) ( 1999, Aug, 28 ) -18 Weeks
            , toTest ( 2000, Jan, 1 ) ( 1999, Dec, 14 ) -18 Days
            ]
        , describe "diffing Years returns a number of whole years as determined by calendar date (anniversary)"
            [ toTest ( 2000, Feb, 29 ) ( 2001, Feb, 28 ) 0 Years
            , toTest ( 2000, Feb, 29 ) ( 2004, Feb, 29 ) 4 Years
            ]
        , describe "diffing Months returns a number of whole months as determined by calendar date"
            [ toTest ( 2000, Jan, 31 ) ( 2000, Feb, 29 ) 0 Months
            , toTest ( 2000, Jan, 31 ) ( 2000, Mar, 31 ) 2 Months
            , toTest ( 2000, Jan, 31 ) ( 2000, Apr, 30 ) 2 Months
            , toTest ( 2000, Jan, 31 ) ( 2001, Feb, 28 ) 12 Months
            ]
        ]


test_floor : Test
test_floor =
    let
        toTest : Interval -> ( Int, Month, Int ) -> ( Int, Month, Int ) -> Test
        toTest interval ( y1, m1, d1 ) (( y2, m2, d2 ) as expected) =
            describe (Debug.toString interval ++ " " ++ Debug.toString ( y1, m1, d1 ))
                [ test ("=> " ++ Debug.toString expected) <|
                    \() -> Date.fromCalendarDate y1 m1 d1 |> Date.floor interval |> equal (Date.fromCalendarDate y2 m2 d2)
                , test "is idempotent" <|
                    \() -> Date.fromCalendarDate y1 m1 d1 |> expectIdempotence (Date.floor interval)
                ]
    in
    describe "floor"
        [ describe "doesn't affect a date that is already at a rounded interval"
            [ toTest Year ( 2000, Jan, 1 ) ( 2000, Jan, 1 )
            , toTest Quarter ( 2000, Jan, 1 ) ( 2000, Jan, 1 )
            , toTest Month ( 2000, Jan, 1 ) ( 2000, Jan, 1 )
            , toTest Week ( 2000, Jan, 3 ) ( 2000, Jan, 3 )
            , toTest Monday ( 2000, Jan, 3 ) ( 2000, Jan, 3 )
            , toTest Tuesday ( 2000, Jan, 4 ) ( 2000, Jan, 4 )
            , toTest Wednesday ( 2000, Jan, 5 ) ( 2000, Jan, 5 )
            , toTest Thursday ( 2000, Jan, 6 ) ( 2000, Jan, 6 )
            , toTest Friday ( 2000, Jan, 7 ) ( 2000, Jan, 7 )
            , toTest Saturday ( 2000, Jan, 1 ) ( 2000, Jan, 1 )
            , toTest Sunday ( 2000, Jan, 2 ) ( 2000, Jan, 2 )
            , toTest Day ( 2000, Jan, 1 ) ( 2000, Jan, 1 )
            ]
        , describe "returns the previous rounded interval"
            [ toTest Year ( 2000, May, 21 ) ( 2000, Jan, 1 )
            , toTest Quarter ( 2000, May, 21 ) ( 2000, Apr, 1 )
            , toTest Month ( 2000, May, 21 ) ( 2000, May, 1 )
            , toTest Week ( 2000, May, 21 ) ( 2000, May, 15 )
            , toTest Monday ( 2000, May, 21 ) ( 2000, May, 15 )
            , toTest Tuesday ( 2000, May, 21 ) ( 2000, May, 16 )
            , toTest Wednesday ( 2000, May, 21 ) ( 2000, May, 17 )
            , toTest Thursday ( 2000, May, 21 ) ( 2000, May, 18 )
            , toTest Friday ( 2000, May, 21 ) ( 2000, May, 19 )
            , toTest Saturday ( 2000, May, 21 ) ( 2000, May, 20 )
            , toTest Sunday ( 2000, May, 22 ) ( 2000, May, 21 )
            , toTest Day ( 2000, May, 21 ) ( 2000, May, 21 )
            ]
        , describe "rounds to Quarter as expected" <|
            List.concatMap
                (\( ms, expected ) -> ms |> List.map (\m -> toTest Quarter ( 2000, m, 15 ) expected))
                [ ( [ Jan, Feb, Mar ], ( 2000, Jan, 1 ) )
                , ( [ Apr, May, Jun ], ( 2000, Apr, 1 ) )
                , ( [ Jul, Aug, Sep ], ( 2000, Jul, 1 ) )
                , ( [ Oct, Nov, Dec ], ( 2000, Oct, 1 ) )
                ]
        ]


test_ceiling : Test
test_ceiling =
    let
        toTest : Interval -> ( Int, Month, Int ) -> ( Int, Month, Int ) -> Test
        toTest interval ( y1, m1, d1 ) (( y2, m2, d2 ) as expected) =
            describe (Debug.toString interval ++ " " ++ Debug.toString ( y1, m1, d1 ))
                [ test ("=> " ++ Debug.toString expected) <|
                    \() -> Date.fromCalendarDate y1 m1 d1 |> Date.ceiling interval |> equal (Date.fromCalendarDate y2 m2 d2)
                , test "is idempotent" <|
                    \() -> Date.fromCalendarDate y1 m1 d1 |> expectIdempotence (Date.ceiling interval)
                ]
    in
    describe "ceiling"
        [ describe "doesn't affect a date that is already at a rounded interval"
            [ toTest Year ( 2000, Jan, 1 ) ( 2000, Jan, 1 )
            , toTest Quarter ( 2000, Jan, 1 ) ( 2000, Jan, 1 )
            , toTest Month ( 2000, Jan, 1 ) ( 2000, Jan, 1 )
            , toTest Week ( 2000, Jan, 3 ) ( 2000, Jan, 3 )
            , toTest Monday ( 2000, Jan, 3 ) ( 2000, Jan, 3 )
            , toTest Tuesday ( 2000, Jan, 4 ) ( 2000, Jan, 4 )
            , toTest Wednesday ( 2000, Jan, 5 ) ( 2000, Jan, 5 )
            , toTest Thursday ( 2000, Jan, 6 ) ( 2000, Jan, 6 )
            , toTest Friday ( 2000, Jan, 7 ) ( 2000, Jan, 7 )
            , toTest Saturday ( 2000, Jan, 1 ) ( 2000, Jan, 1 )
            , toTest Sunday ( 2000, Jan, 2 ) ( 2000, Jan, 2 )
            , toTest Day ( 2000, Jan, 1 ) ( 2000, Jan, 1 )
            ]
        , describe "returns the next rounded interval"
            [ toTest Year ( 2000, May, 21 ) ( 2001, Jan, 1 )
            , toTest Quarter ( 2000, May, 21 ) ( 2000, Jul, 1 )
            , toTest Month ( 2000, May, 21 ) ( 2000, Jun, 1 )
            , toTest Week ( 2000, May, 21 ) ( 2000, May, 22 )
            , toTest Monday ( 2000, May, 21 ) ( 2000, May, 22 )
            , toTest Tuesday ( 2000, May, 21 ) ( 2000, May, 23 )
            , toTest Wednesday ( 2000, May, 21 ) ( 2000, May, 24 )
            , toTest Thursday ( 2000, May, 21 ) ( 2000, May, 25 )
            , toTest Friday ( 2000, May, 21 ) ( 2000, May, 26 )
            , toTest Saturday ( 2000, May, 21 ) ( 2000, May, 27 )
            , toTest Sunday ( 2000, May, 22 ) ( 2000, May, 28 )
            , toTest Day ( 2000, May, 21 ) ( 2000, May, 21 )
            ]
        , describe "rounds to Quarter as expected" <|
            List.concatMap
                (\( ms, expected ) -> ms |> List.map (\m -> toTest Quarter ( 2000, m, 15 ) expected))
                [ ( [ Jan, Feb, Mar ], ( 2000, Apr, 1 ) )
                , ( [ Apr, May, Jun ], ( 2000, Jul, 1 ) )
                , ( [ Jul, Aug, Sep ], ( 2000, Oct, 1 ) )
                , ( [ Oct, Nov, Dec ], ( 2001, Jan, 1 ) )
                ]
        ]


test_range : Test
test_range =
    let
        toTest : Interval -> Int -> CalendarDate -> CalendarDate -> List CalendarDate -> Test
        toTest interval step start end expected =
            test ([ Debug.toString interval, Debug.toString step, Debug.toString start, Debug.toString end ] |> String.join " ") <|
                \() ->
                    Date.range interval step (fromCalendarDate start) (fromCalendarDate end)
                        |> equal (expected |> List.map fromCalendarDate)
    in
    describe "range"
        [ describe "returns a list of dates at rounded intervals which may include start and must exclude end"
            [ toTest Year 10 (CalendarDate 2000 Jan 1) (CalendarDate 2030 Jan 1) <|
                [ CalendarDate 2000 Jan 1
                , CalendarDate 2010 Jan 1
                , CalendarDate 2020 Jan 1
                ]
            , toTest Quarter 1 (CalendarDate 2000 Jan 1) (CalendarDate 2000 Sep 1) <|
                [ CalendarDate 2000 Jan 1
                , CalendarDate 2000 Apr 1
                , CalendarDate 2000 Jul 1
                ]
            , toTest Month 2 (CalendarDate 2000 Jan 1) (CalendarDate 2000 Jul 1) <|
                [ CalendarDate 2000 Jan 1
                , CalendarDate 2000 Mar 1
                , CalendarDate 2000 May 1
                ]
            , toTest Week 2 (CalendarDate 2000 Jan 1) (CalendarDate 2000 Feb 14) <|
                [ CalendarDate 2000 Jan 3
                , CalendarDate 2000 Jan 17
                , CalendarDate 2000 Jan 31
                ]
            , toTest Monday 2 (CalendarDate 2000 Jan 1) (CalendarDate 2000 Feb 14) <|
                [ CalendarDate 2000 Jan 3
                , CalendarDate 2000 Jan 17
                , CalendarDate 2000 Jan 31
                ]
            , toTest Tuesday 2 (CalendarDate 2000 Jan 1) (CalendarDate 2000 Feb 15) <|
                [ CalendarDate 2000 Jan 4
                , CalendarDate 2000 Jan 18
                , CalendarDate 2000 Feb 1
                ]
            , toTest Wednesday 2 (CalendarDate 2000 Jan 1) (CalendarDate 2000 Feb 16) <|
                [ CalendarDate 2000 Jan 5
                , CalendarDate 2000 Jan 19
                , CalendarDate 2000 Feb 2
                ]
            , toTest Thursday 2 (CalendarDate 2000 Jan 1) (CalendarDate 2000 Feb 17) <|
                [ CalendarDate 2000 Jan 6
                , CalendarDate 2000 Jan 20
                , CalendarDate 2000 Feb 3
                ]
            , toTest Friday 2 (CalendarDate 2000 Jan 1) (CalendarDate 2000 Feb 18) <|
                [ CalendarDate 2000 Jan 7
                , CalendarDate 2000 Jan 21
                , CalendarDate 2000 Feb 4
                ]
            , toTest Saturday 2 (CalendarDate 2000 Jan 1) (CalendarDate 2000 Feb 12) <|
                [ CalendarDate 2000 Jan 1
                , CalendarDate 2000 Jan 15
                , CalendarDate 2000 Jan 29
                ]
            , toTest Sunday 2 (CalendarDate 2000 Jan 1) (CalendarDate 2000 Feb 13) <|
                [ CalendarDate 2000 Jan 2
                , CalendarDate 2000 Jan 16
                , CalendarDate 2000 Jan 30
                ]
            , toTest Day 2 (CalendarDate 2000 Jan 1) (CalendarDate 2000 Jan 7) <|
                [ CalendarDate 2000 Jan 1
                , CalendarDate 2000 Jan 3
                , CalendarDate 2000 Jan 5
                ]
            ]
        , describe "begins at interval nearest to start date"
            [ toTest Day 10 (CalendarDate 2000 Jan 1) (CalendarDate 2000 Jan 30) <|
                [ CalendarDate 2000 Jan 1
                , CalendarDate 2000 Jan 11
                , CalendarDate 2000 Jan 21
                ]
            , toTest Day 10 (CalendarDate 2000 Jan 1) (CalendarDate 2000 Jan 31) <|
                [ CalendarDate 2000 Jan 1
                , CalendarDate 2000 Jan 11
                , CalendarDate 2000 Jan 21
                ]
            , toTest Day 10 (CalendarDate 2000 Jan 1) (CalendarDate 2000 Feb 1) <|
                [ CalendarDate 2000 Jan 1
                , CalendarDate 2000 Jan 11
                , CalendarDate 2000 Jan 21
                , CalendarDate 2000 Jan 31
                ]
            ]
        , test "returns a list of days as expected" <|
            \() ->
                Date.range Day 1 (Date.fromCalendarDate 2000 Jan 1) (Date.fromCalendarDate 2001 Jan 1)
                    |> equal (calendarDatesInYear 2000 |> List.map fromCalendarDate)
        , test "can return the empty list" <|
            \() ->
                Date.range Day 1 (Date.fromCalendarDate 2000 Jan 1) (Date.fromCalendarDate 2000 Jan 1)
                    |> equal []
        , describe "can return a large list (tail recursion)"
            [ let
                start =
                    Date.fromCalendarDate 1950 Jan 1

                end =
                    Date.fromCalendarDate 2050 Jan 1

                expectedLength =
                    Date.diff Days start end
              in
              test ("length: " ++ Debug.toString expectedLength) <|
                \() -> Date.range Day 1 start end |> List.length |> equal expectedLength
            ]
        ]


test_fromIsoString : Test
test_fromIsoString =
    let
        toTest : ( String, ( Int, Month, Int ) ) -> Test
        toTest ( string, ( y, m, d ) as expected ) =
            test (string ++ " => " ++ Debug.toString expected) <|
                \() -> Date.fromIsoString string |> equal (Ok (Date.fromCalendarDate y m d))
    in
    describe "fromIsoString"
        [ describe "converts ISO 8601 date strings in basic format" <|
            List.map toTest
                [ ( "2008", ( 2008, Jan, 1 ) )
                , ( "200812", ( 2008, Dec, 1 ) )
                , ( "20081231", ( 2008, Dec, 31 ) )
                , ( "2009W01", ( 2008, Dec, 29 ) )
                , ( "2009W014", ( 2009, Jan, 1 ) )
                , ( "2008061", ( 2008, Mar, 1 ) )
                ]
        , describe "converts ISO 8601 date strings in extended format" <|
            List.map toTest
                [ ( "2008-12", ( 2008, Dec, 1 ) )
                , ( "2008-12-31", ( 2008, Dec, 31 ) )
                , ( "2009-W01", ( 2008, Dec, 29 ) )
                , ( "2009-W01-4", ( 2009, Jan, 1 ) )
                , ( "2008-061", ( 2008, Mar, 1 ) )
                ]
        , describe "returns Err for malformed date strings" <|
            List.map
                (\s -> test s <| \() -> Date.fromIsoString s |> extractErr "" |> String.startsWith "Expected a date" |> equal True)
                [ "200812-31"
                , "2008-1231"
                , "2009W01-4"
                , "2009-W014"
                , "2008-012-31"
                , "2008-12-031"
                , "2008-0061"
                , "2018-05-1"
                , "2018-5"
                , "20180"
                ]
        , describe "returns Err for invalid dates" <|
            List.map
                (\s -> test s <| \() -> Date.fromIsoString s |> extractErr "" |> String.startsWith "Invalid" |> equal True)
                [ "2008-00"
                , "2008-13"
                , "2008-00-01"
                , "2008-13-01"
                , "2008-01-00"
                , "2008-01-32"
                , "2007-02-29"
                , "2008-02-30"
                , "2008-W00-1"
                , "2008-W53-1"
                , "2008-W01-0"
                , "2008-W01-8"
                , "2008-000"
                , "2007-366"
                , "2008-367"
                ]
        , describe "returns Err for a valid date followed by a 'T'" <|
            List.map
                (\s -> test s <| \() -> Date.fromIsoString s |> equal (Err "Expected a date only, not a date and time"))
                [ "2018-09-26T00:00:00.000Z"
                , "2018-W39-3T00:00:00.000Z"
                , "2018-269T00:00:00.000Z"
                ]
        , describe "returns Err for a valid date followed by anything else" <|
            List.map
                (\s -> test s <| \() -> Date.fromIsoString s |> equal (Err "Expected a date only"))
                [ "2018-09-26 "
                , "2018-W39-3 "
                , "2018-269 "
                ]
        , describe "can form an isomorphism with toIsoString"
            (List.concat
                [ List.range 1897 1905
                , List.range 1997 2025
                , List.range -5 5
                ]
                |> List.concatMap calendarDatesInYear
                |> List.map
                    (\calendarDate ->
                        test (Debug.toString calendarDate) <|
                            \() ->
                                expectIsomorphism
                                    (Result.map Date.toIsoString)
                                    (Result.andThen Date.fromIsoString)
                                    (Ok <| fromCalendarDate calendarDate)
                    )
            )
        , describe "can form an isomorphism with `format \"yyyy-DDD\"`"
            (List.concat
                [ List.range 1997 2005
                , List.range -5 5
                ]
                |> List.concatMap calendarDatesInYear
                |> List.map
                    (\calendarDate ->
                        test (Debug.toString calendarDate) <|
                            \() ->
                                expectIsomorphism
                                    (Result.map (Date.format "yyyy-DDD"))
                                    (Result.andThen Date.fromIsoString)
                                    (Ok <| fromCalendarDate calendarDate)
                    )
            )
        , describe "can form an isomorphism with `format \"YYYY-'W'ww-e\"`"
            (List.concat
                [ List.range 1997 2005
                , List.range -5 5
                ]
                |> List.concatMap calendarDatesInYear
                |> List.map
                    (\calendarDate ->
                        test (Debug.toString calendarDate) <|
                            \() ->
                                expectIsomorphism
                                    (Result.map (Date.format "YYYY-'W'ww-e"))
                                    (Result.andThen Date.fromIsoString)
                                    (Ok <| fromCalendarDate calendarDate)
                    )
            )
        ]


test_fromOrdinalDate : Test
test_fromOrdinalDate =
    describe "fromOrdinalDate"
        [ describe "clamps days that are out of range for the given year"
            (List.map
                (\( ( y, od ), expected ) ->
                    test (Debug.toString ( y, od ) ++ " " ++ Debug.toString expected) <|
                        \() ->
                            Date.fromOrdinalDate y od |> toOrdinalDate |> equal expected
                )
                [ ( ( 2000, -1 ), OrdinalDate 2000 1 )
                , ( ( 2000, 0 ), OrdinalDate 2000 1 )
                , ( ( 2001, 366 ), OrdinalDate 2001 365 )
                , ( ( 2000, 367 ), OrdinalDate 2000 366 )
                ]
            )
        ]


test_fromCalendarDate : Test
test_fromCalendarDate =
    describe "fromCalendarDate"
        [ describe "clamps days that are out of range for the given year and month"
            (List.map
                (\( ( y, m, d ), expected ) ->
                    test (Debug.toString ( y, m, d ) ++ " " ++ Debug.toString expected) <|
                        \() ->
                            Date.fromCalendarDate y m d |> toCalendarDate |> equal expected
                )
                [ ( ( 2000, Jan, -1 ), CalendarDate 2000 Jan 1 )
                , ( ( 2000, Jan, 0 ), CalendarDate 2000 Jan 1 )
                , ( ( 2000, Jan, 32 ), CalendarDate 2000 Jan 31 )
                , ( ( 2000, Feb, 0 ), CalendarDate 2000 Feb 1 )
                , ( ( 2001, Feb, 29 ), CalendarDate 2001 Feb 28 )
                , ( ( 2000, Feb, 30 ), CalendarDate 2000 Feb 29 )
                , ( ( 2000, Mar, 32 ), CalendarDate 2000 Mar 31 )
                , ( ( 2000, Apr, 31 ), CalendarDate 2000 Apr 30 )
                , ( ( 2000, May, 32 ), CalendarDate 2000 May 31 )
                , ( ( 2000, Jun, 31 ), CalendarDate 2000 Jun 30 )
                , ( ( 2000, Jul, 32 ), CalendarDate 2000 Jul 31 )
                , ( ( 2000, Aug, 32 ), CalendarDate 2000 Aug 31 )
                , ( ( 2000, Sep, 31 ), CalendarDate 2000 Sep 30 )
                , ( ( 2000, Oct, 32 ), CalendarDate 2000 Oct 31 )
                , ( ( 2000, Nov, 31 ), CalendarDate 2000 Nov 30 )
                , ( ( 2000, Dec, 32 ), CalendarDate 2000 Dec 31 )
                ]
            )
        ]


test_fromWeekDate : Test
test_fromWeekDate =
    describe "fromWeekDate"
        [ describe "clamps weeks that are out of range for the given week-year"
            (List.map
                (\( ( wy, wn, wd ), expected ) ->
                    test (Debug.toString ( wy, wn, wd ) ++ " " ++ Debug.toString expected) <|
                        \() ->
                            Date.fromWeekDate wy wn wd |> toWeekDate |> equal expected
                )
                [ ( ( 2000, -1, Mon ), WeekDate 2000 1 Mon )
                , ( ( 2000, 0, Mon ), WeekDate 2000 1 Mon )
                , ( ( 2000, 53, Mon ), WeekDate 2000 52 Mon )
                , ( ( 2004, 54, Mon ), WeekDate 2004 53 Mon )
                ]
            )
        ]


test_numberToMonth : Test
test_numberToMonth =
    describe "numberToMonth"
        [ describe "clamps numbers that are out of range"
            (List.map
                (\( n, month ) ->
                    test (Debug.toString ( n, month )) <| \() -> n |> Date.numberToMonth |> equal month
                )
                [ ( -1, Jan )
                , ( 0, Jan )
                , ( 13, Dec )
                ]
            )
        ]


test_numberToWeekday : Test
test_numberToWeekday =
    describe "numberToWeekday"
        [ describe "clamps numbers that are out of range"
            (List.map
                (\( n, weekday ) ->
                    test (Debug.toString ( n, weekday )) <| \() -> n |> Date.numberToWeekday |> equal weekday
                )
                [ ( -1, Mon )
                , ( 0, Mon )
                , ( 8, Sun )
                ]
            )
        ]



{-
   test_is53WeekYear : Test
   test_is53WeekYear =
       test "is53WeekYear" <|
           \() ->
               List.range 1970 2040
                   |> List.filter Date.is53WeekYear
                   |> equal [ 1970, 1976, 1981, 1987, 1992, 1998, 2004, 2009, 2015, 2020, 2026, 2032, 2037 ]
-}


test_compare : Test
test_compare =
    describe "compare"
        [ describe "returns an Order" <|
            List.map
                (\( a, b, expected ) ->
                    test (Debug.toString a ++ " " ++ Debug.toString b) <|
                        \() -> Date.compare a b |> equal expected
                )
                [ ( Date.fromOrdinalDate 1970 1, Date.fromOrdinalDate 2038 1, LT )
                , ( Date.fromOrdinalDate 1970 1, Date.fromOrdinalDate 1970 1, EQ )
                , ( Date.fromOrdinalDate 2038 1, Date.fromOrdinalDate 1970 1, GT )
                ]
        , test "can be used with List.sortWith" <|
            \() ->
                [ Date.fromOrdinalDate 2038 1
                , Date.fromOrdinalDate 2038 19
                , Date.fromOrdinalDate 1970 1
                , Date.fromOrdinalDate 1969 201
                , Date.fromOrdinalDate 2001 1
                ]
                    |> List.sortWith Date.compare
                    |> equal
                        [ Date.fromOrdinalDate 1969 201
                        , Date.fromOrdinalDate 1970 1
                        , Date.fromOrdinalDate 2001 1
                        , Date.fromOrdinalDate 2038 1
                        , Date.fromOrdinalDate 2038 19
                        ]
        ]


test_isBetween : Test
test_isBetween =
    let
        ( a, b, c ) =
            ( Date.fromOrdinalDate 1969 201
            , Date.fromOrdinalDate 1970 1
            , Date.fromOrdinalDate 2038 19
            )

        toTest : ( String, ( Date, Date, Date ), Bool ) -> Test
        toTest ( desc, ( minimum, maximum, x ), expected ) =
            test desc <|
                \() ->
                    Date.isBetween minimum maximum x |> equal expected
    in
    describe "isBetween"
        [ describe "when min < max, works as expected" <|
            List.map toTest
                [ ( "before", ( b, c, a ), False )
                , ( "min", ( b, c, b ), True )
                , ( "middle", ( a, c, b ), True )
                , ( "max", ( a, b, b ), True )
                , ( "after", ( a, b, c ), False )
                ]
        , describe "when min == max, works as expected" <|
            List.map toTest
                [ ( "before", ( b, b, a ), False )
                , ( "equal", ( b, b, b ), True )
                , ( "after", ( b, b, c ), False )
                ]
        , describe "when min > max, always returns False" <|
            List.map toTest
                [ ( "before", ( c, b, a ), False )
                , ( "min", ( c, b, b ), False )
                , ( "middle", ( c, a, b ), False )
                , ( "max", ( b, a, b ), False )
                , ( "after", ( b, a, c ), False )
                ]
        ]


test_min : Test
test_min =
    let
        ( a, b ) =
            ( Date.fromOrdinalDate 1969 201
            , Date.fromOrdinalDate 1970 1
            )
    in
    describe "min"
        [ test "a < b" <| \() -> Date.min a b |> equal a
        , test "b < a" <| \() -> Date.min b a |> equal a
        ]


test_max : Test
test_max =
    let
        ( a, b ) =
            ( Date.fromOrdinalDate 1969 201
            , Date.fromOrdinalDate 1970 1
            )
    in
    describe "max"
        [ test "a < b" <| \() -> Date.max a b |> equal b
        , test "b < a" <| \() -> Date.max b a |> equal b
        ]


test_clamp : Test
test_clamp =
    let
        ( a, b, c ) =
            ( Date.fromOrdinalDate 1969 201
            , Date.fromOrdinalDate 1970 1
            , Date.fromOrdinalDate 2038 19
            )

        toTest : ( String, ( Date, Date, Date ), Date ) -> Test
        toTest ( desc, ( minimum, maximum, x ), expected ) =
            test desc <|
                \() ->
                    Date.clamp minimum maximum x |> equal expected
    in
    describe "clamp"
        [ describe "when min < max, works as expected" <|
            List.map toTest
                [ ( "before", ( b, c, a ), b )
                , ( "min", ( b, c, b ), b )
                , ( "middle", ( a, c, b ), b )
                , ( "max", ( a, b, b ), b )
                , ( "after", ( a, b, c ), b )
                ]
        , describe "when min == max, works as expected" <|
            List.map toTest
                [ ( "before", ( b, b, a ), b )
                , ( "equal", ( b, b, b ), b )
                , ( "after", ( b, b, c ), b )
                ]
        ]



-- records


type alias OrdinalDate =
    { year : Int, ordinalDay : Int }


toOrdinalDate : Date -> OrdinalDate
toOrdinalDate date =
    OrdinalDate
        (date |> Date.year)
        (date |> Date.ordinalDay)


type alias CalendarDate =
    { year : Int, month : Month, day : Int }


fromCalendarDate : CalendarDate -> Date
fromCalendarDate { year, month, day } =
    Date.fromCalendarDate year month day


toCalendarDate : Date -> CalendarDate
toCalendarDate date =
    CalendarDate
        (date |> Date.year)
        (date |> Date.month)
        (date |> Date.day)


type alias WeekDate =
    { weekYear : Int, weekNumber : Int, weekday : Weekday }


fromWeekDate : WeekDate -> Date
fromWeekDate { weekYear, weekNumber, weekday } =
    Date.fromWeekDate weekYear weekNumber weekday


toWeekDate : Date -> WeekDate
toWeekDate date =
    WeekDate
        (date |> Date.weekYear)
        (date |> Date.weekNumber)
        (date |> Date.weekday)



-- dates


calendarDatesInYear : Int -> List CalendarDate
calendarDatesInYear y =
    [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]
        |> List.concatMap
            (\m -> List.range 1 (daysInMonth y m) |> List.map (CalendarDate y m))


isLeapYear : Int -> Bool
isLeapYear y =
    modBy 4 y == 0 && modBy 100 y /= 0 || modBy 400 y == 0


daysInMonth : Int -> Month -> Int
daysInMonth y m =
    case m of
        Jan ->
            31

        Feb ->
            if isLeapYear y then
                29

            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31



-- result


extractErr : x -> Result x a -> x
extractErr default result =
    case result of
        Err x ->
            x

        Ok _ ->
            default



-- expectation


expectIsomorphism : (x -> y) -> (y -> x) -> x -> Expectation
expectIsomorphism xToY yToX x =
    x |> xToY |> yToX |> equal x


expectIdempotence : (x -> x) -> x -> Expectation
expectIdempotence f x =
    f (f x) |> equal (f x)
