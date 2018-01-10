module Tests exposing (..)

import Date.RataDie as Date exposing (Month(..), Weekday(..))
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


type alias Date =
    Int


testCalendarDate : Test
testCalendarDate =
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
        , test "fromCalendarDate produces a contiguous list of integers from a contiguous list of calendar dates" <|
            \() ->
                List.range 1997 2025
                    |> List.concatMap (calendarDatesInYear >> List.map fromCalendarDate)
                    |> Expect.equal (List.range (Date.fromCalendarDate 1997 Jan 1) (Date.fromCalendarDate 2025 Dec 31))
        ]


testWeekDate : Test
testWeekDate =
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
                (testDateToFormattedString (Date.fromCalendarDate 2001 Jan 2))
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
                (testDateToFormattedString (Date.fromCalendarDate 2008 Dec 31))
                [ ( "ABCFGHIJKLNOPRSTUVWXZabcfghijklmnopqrstuvxz", "ABCFGHIJKLNOPRSTUVWXZabcfghijklmnopqrstuvxz" )
                , ( "0123456789", "0123456789" )
                ]
        , describe "handles escaped characters and escaped escape characters" <|
            List.map
                (testDateToFormattedString (Date.fromCalendarDate 2001 Jan 2))
                [ ( "'yYQMwdDEe'", "yYQMwdDEe" )
                , ( "''' '' ''' ''", "' ' ' '" )
                , ( "'yyyy:' yyyy", "yyyy: 2001" )
                ]
        , describe "formats day ordinals" <|
            List.map
                (\( n, string ) ->
                    testDateToFormattedString (Date.fromCalendarDate 2001 Jan n) ( "ddd", string )
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
                (testDateToFormattedString (Date.fromCalendarDate 2008 Dec 31))
                [ ( "yyyy-MM-dd", "2008-12-31" )
                , ( "yyyy-DDD", "2008-366" )
                , ( "YYYY-'W'ww-e", "2009-W01-3" )
                , ( "M/d/y", "12/31/2008" )
                , ( "''yy", "'08" )
                ]
        ]



-- helpers


type alias CalendarDate =
    { year : Int, month : Month, day : Int }


fromCalendarDate : CalendarDate -> Date
fromCalendarDate { year, month, day } =
    Date.fromCalendarDate year month day


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
    { weekYear : Int, week : Int, weekday : Weekday }


fromWeekDate : WeekDate -> Date
fromWeekDate { weekYear, week, weekday } =
    Date.fromWeekDate weekYear week weekday



--


expectIsomorphism : (x -> y) -> (y -> x) -> x -> Expectation
expectIsomorphism xToY yToX x =
    x |> xToY |> yToX |> Expect.equal x
