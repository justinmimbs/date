module Tests exposing (..)

import Date.RataDie as Date
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


type alias Date =
    Int


testCalendarDate : Test
testCalendarDate =
    describe "[from|to]CalendarDate"
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


testWeekDate : Test
testWeekDate =
    describe "[to|from]WeekDate"
        (List.range 1997 2025
            |> List.concatMap calendarDatesInYear
            |> List.map
                (\calendarDate ->
                    test (toString calendarDate) <|
                        \() -> expectIsomorphism Date.toWeekDate fromWeekDate (fromCalendarDate calendarDate)
                )
        )



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
    Expect.equal x (x |> xToY |> yToX)
