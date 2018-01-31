module Tests exposing (..)

import Date.RataDie as Date exposing (Interval(..), Month(..), Unit(..), Weekday(..))
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


type alias Date =
    Int


test_CalendarDate : Test
test_CalendarDate =
    describe "CalendarDate"
        [ describe "CalendarDate and Date are are isomorphic"
            (List.concat
                [ List.range 1897 1905
                , List.range 1997 2025
                ]
                |> List.concatMap calendarDatesInYear
                |> List.map
                    (\calendarDate ->
                        test (toString calendarDate) <|
                            \() -> expectIsomorphism fromCalendarDate Date.toCalendarDate calendarDate
                    )
            )
        , test "A list of contiguous CalendarDates is equivalent to a list of contiguous integers" <|
            \() ->
                List.range 1997 2025
                    |> List.concatMap (calendarDatesInYear >> List.map fromCalendarDate)
                    |> Expect.equal (List.range (fromCalendarParts 1997 Jan 1) (fromCalendarParts 2025 Dec 31))
        ]


test_WeekDate : Test
test_WeekDate =
    describe "WeekDate"
        [ describe "WeekDate and Date are isomorphic"
            (List.range 1997 2025
                |> List.concatMap calendarDatesInYear
                |> List.map
                    (\calendarDate ->
                        test (toString calendarDate) <|
                            \() -> expectIsomorphism Date.toWeekDate fromWeekDate (fromCalendarDate calendarDate)
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
                        test (toString calendarDate) <|
                            \() -> fromCalendarDate calendarDate |> Date.toWeekDate |> Expect.equal weekDate
                    )
            )
        ]


test_toFormattedString : Test
test_toFormattedString =
    let
        testDateToFormattedString : Date -> ( String, String ) -> Test
        testDateToFormattedString date ( pattern, expected ) =
            test ("\"" ++ pattern ++ "\" " ++ toString date) <|
                \() -> date |> Date.toFormattedString pattern |> Expect.equal expected
    in
    describe "toFormattedString"
        [ describe "replaces supported character patterns" <|
            List.map
                (testDateToFormattedString (fromCalendarParts 2001 Jan 2))
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
        , describe "ignores unsupported character patterns" <|
            List.map
                (testDateToFormattedString (fromCalendarParts 2008 Dec 31))
                [ ( "ABCFGHIJKLNOPRSTUVWXZabcfghijklmnopqrstuvxz", "ABCFGHIJKLNOPRSTUVWXZabcfghijklmnopqrstuvxz" )
                , ( "0123456789", "0123456789" )
                ]
        , describe "handles escaped characters and escaped escape characters" <|
            List.map
                (testDateToFormattedString (fromCalendarParts 2001 Jan 2))
                [ ( "'yYQMwdDEe'", "yYQMwdDEe" )
                , ( "''' '' ''' ''", "' ' ' '" )
                , ( "'yyyy:' yyyy", "yyyy: 2001" )
                ]
        , describe "formats day ordinals" <|
            List.map
                (\( n, string ) ->
                    testDateToFormattedString (fromCalendarParts 2001 Jan n) ( "ddd", string )
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
                (testDateToFormattedString (fromCalendarParts 2008 Dec 31))
                [ ( "yyyy-MM-dd", "2008-12-31" )
                , ( "yyyy-DDD", "2008-366" )
                , ( "YYYY-'W'ww-e", "2009-W01-3" )
                , ( "M/d/y", "12/31/2008" )
                , ( "''yy", "'08" )
                ]
        ]


test_add : Test
test_add =
    let
        toTest : ( Int, Month, Int ) -> Int -> Unit -> ( Int, Month, Int ) -> Test
        toTest ( y1, m1, d1 ) n unit (( y2, m2, d2 ) as expected) =
            test (toString ( y1, m1, d1 ) ++ " + " ++ toString n ++ " " ++ toString unit ++ " => " ++ toString expected) <|
                \() ->
                    fromCalendarParts y1 m1 d1 |> Date.add unit n |> Expect.equal (fromCalendarParts y2 m2 d2)
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
            test (toString ( y2, m2, d2 ) ++ " - " ++ toString ( y1, m1, d1 ) ++ " => " ++ toString expected ++ " " ++ toString unit) <|
                \() ->
                    Date.diff unit (fromCalendarParts y1 m1 d1) (fromCalendarParts y2 m2 d2) |> Expect.equal expected
    in
    describe "diff"
        [ describe "diff x x == 0" <|
            List.map
                (\unit -> toTest ( 2000, Jan, 1 ) ( 2000, Jan, 1 ) 0 unit)
                [ Years, Months, Weeks, Days ]
        , describe "diff x y == -(diff y x)" <|
            let
                ( x, y ) =
                    ( fromCalendarParts 2000 Jan 1, fromCalendarParts 2017 Sep 28 )
            in
            List.map
                (\unit -> test (toString unit) <| \() -> Date.diff unit x y |> Expect.equal (negate (Date.diff unit y x)))
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
            describe (toString interval ++ " " ++ toString ( y1, m1, d1 ))
                [ test ("=> " ++ toString expected) <|
                    \() -> fromCalendarParts y1 m1 d1 |> Date.floor interval |> Expect.equal (fromCalendarParts y2 m2 d2)
                , test "is idempotent" <|
                    \() -> fromCalendarParts y1 m1 d1 |> expectIdempotence (Date.floor interval)
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
            describe (toString interval ++ " " ++ toString ( y1, m1, d1 ))
                [ test ("=> " ++ toString expected) <|
                    \() -> fromCalendarParts y1 m1 d1 |> Date.ceiling interval |> Expect.equal (fromCalendarParts y2 m2 d2)
                , test "is idempotent" <|
                    \() -> fromCalendarParts y1 m1 d1 |> expectIdempotence (Date.ceiling interval)
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
            test ([ toString interval, toString step, toString start, toString end ] |> String.join " ") <|
                \() ->
                    Date.range interval step (fromCalendarDate start) (fromCalendarDate end)
                        |> Expect.equal (expected |> List.map fromCalendarDate)
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
                Date.range Day 1 (fromCalendarParts 2000 Jan 1) (fromCalendarParts 2001 Jan 1)
                    |> Expect.equal (calendarDatesInYear 2000 |> List.map fromCalendarDate)
        , test "can return the empty list" <|
            \() ->
                Date.range Day 1 (fromCalendarParts 2000 Jan 1) (fromCalendarParts 2000 Jan 1)
                    |> Expect.equal []
        , describe "can return a large list (tail recursion)"
            [ let
                start =
                    fromCalendarParts 1950 Jan 1

                end =
                    fromCalendarParts 2050 Jan 1

                expectedLength =
                    Date.diff Days start end
              in
              test ("length: " ++ toString expectedLength) <|
                \() -> Date.range Day 1 start end |> List.length |> Expect.equal expectedLength
            ]
        ]


test_fromIsoString : Test
test_fromIsoString =
    let
        toTest : ( String, ( Int, Month, Int ) ) -> Test
        toTest ( string, ( y, m, d ) as expected ) =
            test (string ++ " => " ++ toString expected) <|
                \() -> Date.fromIsoString string |> Expect.equal (Ok (fromCalendarParts y m d))
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
                (\s -> test s <| \() -> Date.fromIsoString s |> Expect.equal (Err "String is not in IS0 8601 date format"))
                [ "200812-31"
                , "2008-1231"
                , "2009W01-4"
                , "2009-W014"
                , "2008-012-31"
                , "2008-12-031"
                , "2008-0061"
                ]
        , describe "returns Err for invalid dates" <|
            List.map
                (\s -> test s <| \() -> Date.fromIsoString s |> extractErr "" |> String.startsWith "Invalid" |> Expect.true "")
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
        , describe "can form an isomorphism with toIsoString"
            (List.concat
                [ List.range 1897 1905
                , List.range 1997 2025
                ]
                |> List.concatMap calendarDatesInYear
                |> List.map
                    (\calendarDate ->
                        test (toString calendarDate) <|
                            \() ->
                                expectIsomorphism
                                    (Result.map Date.toIsoString)
                                    (Result.andThen Date.fromIsoString)
                                    (Ok <| fromCalendarDate calendarDate)
                    )
            )
        , describe "can form an isomorphism with `toFormattedString \"yyyy-DDD\"`"
            (List.concat
                [ List.range 1997 2005
                ]
                |> List.concatMap calendarDatesInYear
                |> List.map
                    (\calendarDate ->
                        test (toString calendarDate) <|
                            \() ->
                                expectIsomorphism
                                    (Result.map (Date.toFormattedString "yyyy-DDD"))
                                    (Result.andThen Date.fromIsoString)
                                    (Ok <| fromCalendarDate calendarDate)
                    )
            )
        , describe "can form an isomorphism with `toFormattedString \"YYYY-'W'ww-e\"`"
            (List.concat
                [ List.range 1997 2005
                ]
                |> List.concatMap calendarDatesInYear
                |> List.map
                    (\calendarDate ->
                        test (toString calendarDate) <|
                            \() ->
                                expectIsomorphism
                                    (Result.map (Date.toFormattedString "YYYY-'W'ww-e"))
                                    (Result.andThen Date.fromIsoString)
                                    (Ok <| fromCalendarDate calendarDate)
                    )
            )
        ]



{-
   test_is53WeekYear : Test
   test_is53WeekYear =
       test "is53WeekYear" <|
           \() ->
               List.range 1970 2040
                   |> List.filter Date.is53WeekYear
                   |> Expect.equal [ 1970, 1976, 1981, 1987, 1992, 1998, 2004, 2009, 2015, 2020, 2026, 2032, 2037 ]
-}
-- helpers


type alias CalendarDate =
    { year : Int, month : Month, day : Int }


fromCalendarDate : CalendarDate -> Date
fromCalendarDate { year, month, day } =
    fromCalendarParts year month day


fromCalendarParts : Int -> Month -> Int -> Date
fromCalendarParts year month day =
    Date.firstOfMonth year month |> Date.add Days (day - 1)


calendarDatesInYear : Int -> List CalendarDate
calendarDatesInYear y =
    [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]
        |> List.concatMap
            (\m -> List.range 1 (daysInMonth y m) |> List.map (CalendarDate y m))


isLeapYear : Int -> Bool
isLeapYear y =
    y % 4 == 0 && y % 100 /= 0 || y % 400 == 0


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


type alias WeekDate =
    { weekYear : Int, weekNumber : Int, weekday : Weekday }


fromWeekDate : WeekDate -> Date
fromWeekDate { weekYear, weekNumber, weekday } =
    fromWeekParts weekYear weekNumber weekday


fromWeekParts : Int -> Int -> Weekday -> Date
fromWeekParts weekYear weekNumber weekday =
    let
        weekdayNumber =
            case weekday of
                Mon ->
                    1

                Tue ->
                    2

                Wed ->
                    3

                Thu ->
                    4

                Fri ->
                    5

                Sat ->
                    6

                Sun ->
                    7
    in
    Date.firstOfWeekYear weekYear |> Date.add Weeks (weekNumber - 1) |> Date.add Days (weekdayNumber - 1)


extractErr : x -> Result x a -> x
extractErr default result =
    case result of
        Err x ->
            x

        Ok _ ->
            default



--


expectIsomorphism : (x -> y) -> (y -> x) -> x -> Expectation
expectIsomorphism xToY yToX x =
    x |> xToY |> yToX |> Expect.equal x


expectIdempotence : (x -> x) -> x -> Expectation
expectIdempotence f x =
    f (f x) |> Expect.equal (f x)
