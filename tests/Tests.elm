module Tests exposing (..)

import Date.RataDie as Date
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
                    |> Expect.equal (List.range (Date.fromCalendarDate 1997 1 1) (Date.fromCalendarDate 2025 12 31))
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
            ([ ( CalendarDate 2005 1 1, WeekDate 2004 53 6 )
             , ( CalendarDate 2005 1 2, WeekDate 2004 53 7 )
             , ( CalendarDate 2005 12 31, WeekDate 2005 52 6 )
             , ( CalendarDate 2007 1 1, WeekDate 2007 1 1 )
             , ( CalendarDate 2007 12 30, WeekDate 2007 52 7 )
             , ( CalendarDate 2007 12 31, WeekDate 2008 1 1 )
             , ( CalendarDate 2008 1 1, WeekDate 2008 1 2 )
             , ( CalendarDate 2008 12 28, WeekDate 2008 52 7 )
             , ( CalendarDate 2008 12 29, WeekDate 2009 1 1 )
             , ( CalendarDate 2008 12 30, WeekDate 2009 1 2 )
             , ( CalendarDate 2008 12 31, WeekDate 2009 1 3 )
             , ( CalendarDate 2009 1 1, WeekDate 2009 1 4 )
             , ( CalendarDate 2009 12 31, WeekDate 2009 53 4 )
             , ( CalendarDate 2010 1 1, WeekDate 2009 53 5 )
             , ( CalendarDate 2010 1 2, WeekDate 2009 53 6 )
             , ( CalendarDate 2010 1 3, WeekDate 2009 53 7 )
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
    { year : Int, month : Int, day : Int }


fromCalendarDate : CalendarDate -> Date
fromCalendarDate { year, month, day } =
    Date.fromCalendarDate year month day


calendarDatesInYear : Int -> List CalendarDate
calendarDatesInYear y =
    List.range 1 12
        |> List.concatMap
            (\m -> List.range 1 (daysInMonth y m) |> List.map (CalendarDate y m))


isLeapYear : Int -> Bool
isLeapYear y =
    y % 4 == 0 && y % 100 /= 0 || y % 400 == 0


daysInMonth : Int -> Int -> Int
daysInMonth y m =
    case m % 12 of
        1 ->
            31

        2 ->
            if isLeapYear y then
                29
            else
                28

        3 ->
            31

        4 ->
            30

        5 ->
            31

        6 ->
            30

        7 ->
            31

        8 ->
            31

        9 ->
            30

        10 ->
            31

        11 ->
            30

        _ ->
            31


type alias WeekDate =
    { weekYear : Int, week : Int, weekday : Int }


fromWeekDate : WeekDate -> Date
fromWeekDate { weekYear, week, weekday } =
    Date.fromWeekDate weekYear week weekday



--


expectIsomorphism : (x -> y) -> (y -> x) -> x -> Expectation
expectIsomorphism xToY yToX x =
    x |> xToY |> yToX |> Expect.equal x
