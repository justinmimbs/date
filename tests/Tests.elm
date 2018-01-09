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
