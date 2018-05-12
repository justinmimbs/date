module Date.Basic
    exposing
        ( Date
        , Interval(..)
        , Month(..)
        , Unit(..)
        , Weekday(..)
        , add
        , ceiling
        , day
        , diff
        , floor
        , fromCalendarDate
        , fromIsoString
        , fromOrdinalDate
        , fromRataDie
        , fromWeekDate
        , month
        , monthNumber
        , monthToNumber
        , numberToMonth
        , numberToWeekday
        , ordinalDay
        , quarter
        , range
        , toCalendarDate
        , toFormattedString
        , toIsoString
        , toOrdinalDate
        , toRataDie
        , toWeekDate
        , weekNumber
        , weekYear
        , weekday
        , weekdayNumber
        , weekdayToNumber
        , year
        )

{-|

@docs Date, Month, Weekday


## Constructors

@docs fromCalendarDate, fromOrdinalDate, fromWeekDate


## Formatting

@docs toFormattedString


## ISO 8601

@docs fromIsoString, toIsoString


## Arithmetic

@docs Unit, add, diff


## Rounding

@docs Interval, ceiling, floor


## Lists

@docs range


## Extractions

@docs year, quarter, month, monthNumber, ordinalDay, day, weekYear, weekNumber, weekday, weekdayNumber


## Records

Convenience functions for converting dates to records.

@docs toCalendarDate, toOrdinalDate, toWeekDate


## Month and Weekday helpers

@docs monthToNumber, numberToMonth, weekdayToNumber, numberToWeekday


## Rata Die

Convert a `Date` to and from a raw `Int` representing the date in
[Rata Die](https://en.wikipedia.org/wiki/Rata_Die).

@docs toRataDie, fromRataDie

-}

import Date.RataDie as RataDie exposing (RataDie)


{-| Represents a date without a time or zone.
-}
type Date
    = RD Int


{-| -}
toRataDie : Date -> Int
toRataDie (RD rd) =
    rd


{-| -}
fromRataDie : Int -> Date
fromRataDie rd =
    RD rd



--


{-| -}
type Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec


{-| -}
type Weekday
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun


{-| -}
type Unit
    = Years
    | Months
    | Weeks
    | Days


{-| -}
type Interval
    = Year
    | Quarter
    | Month
    | Week
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    | Day


exportMonth : Month -> RataDie.Month
exportMonth m =
    case m of
        Jan ->
            RataDie.Jan

        Feb ->
            RataDie.Feb

        Mar ->
            RataDie.Mar

        Apr ->
            RataDie.Apr

        May ->
            RataDie.May

        Jun ->
            RataDie.Jun

        Jul ->
            RataDie.Jul

        Aug ->
            RataDie.Aug

        Sep ->
            RataDie.Sep

        Oct ->
            RataDie.Oct

        Nov ->
            RataDie.Nov

        Dec ->
            RataDie.Dec


importMonth : RataDie.Month -> Month
importMonth m =
    case m of
        RataDie.Jan ->
            Jan

        RataDie.Feb ->
            Feb

        RataDie.Mar ->
            Mar

        RataDie.Apr ->
            Apr

        RataDie.May ->
            May

        RataDie.Jun ->
            Jun

        RataDie.Jul ->
            Jul

        RataDie.Aug ->
            Aug

        RataDie.Sep ->
            Sep

        RataDie.Oct ->
            Oct

        RataDie.Nov ->
            Nov

        RataDie.Dec ->
            Dec


exportWeekday : Weekday -> RataDie.Weekday
exportWeekday wd =
    case wd of
        Mon ->
            RataDie.Mon

        Tue ->
            RataDie.Tue

        Wed ->
            RataDie.Wed

        Thu ->
            RataDie.Thu

        Fri ->
            RataDie.Fri

        Sat ->
            RataDie.Sat

        Sun ->
            RataDie.Sun


importWeekday : RataDie.Weekday -> Weekday
importWeekday wd =
    case wd of
        RataDie.Mon ->
            Mon

        RataDie.Tue ->
            Tue

        RataDie.Wed ->
            Wed

        RataDie.Thu ->
            Thu

        RataDie.Fri ->
            Fri

        RataDie.Sat ->
            Sat

        RataDie.Sun ->
            Sun


exportUnit : Unit -> RataDie.Unit
exportUnit unit =
    case unit of
        Years ->
            RataDie.Years

        Months ->
            RataDie.Months

        Weeks ->
            RataDie.Weeks

        Days ->
            RataDie.Days


exportInterval : Interval -> RataDie.Interval
exportInterval interval =
    case interval of
        Year ->
            RataDie.Year

        Quarter ->
            RataDie.Quarter

        Month ->
            RataDie.Month

        Week ->
            RataDie.Week

        Monday ->
            RataDie.Monday

        Tuesday ->
            RataDie.Tuesday

        Wednesday ->
            RataDie.Wednesday

        Thursday ->
            RataDie.Thursday

        Friday ->
            RataDie.Friday

        Saturday ->
            RataDie.Saturday

        Sunday ->
            RataDie.Sunday

        Day ->
            RataDie.Day



--


{-| Move a date by some number of units.

    fromCalendarDate 2018 Sep 26
        |> add Weeks -2
    -- fromCalendarDate 2018 Sep 12

When adding `Years` or `Months`, day values are clamped to the end of the
month if necessary.

    fromCalendarDate 2000 Jan 31
        |> add Months 1
    -- fromCalendarDate 2000 Feb 29

-}
add : Unit -> Int -> Date -> Date
add unit n (RD rd) =
    RD <| RataDie.add (exportUnit unit) n rd


{-| Round up a date to the beginning of the closest interval. The resulting
date will be greater than or equal to the one provided.

    fromCalendarDate 2018 May 11
        |> ceiling Tuesday
    -- fromCalendarDate 2018 May 15

-}
ceiling : Interval -> Date -> Date
ceiling interval (RD rd) =
    RD <| RataDie.ceiling (exportInterval interval) rd


{-| Extracts the day of the month.
-}
day : Date -> Int
day (RD rd) =
    RataDie.day rd


{-| Find the difference, as a number of some units, between two dates.

    diff Months
        (fromCalendarDate 2007 Mar 15)
        (fromCalendarDate 2007 Sep 1)
    -- 5

-}
diff : Unit -> Date -> Date -> Int
diff unit (RD rd1) (RD rd2) =
    RataDie.diff (exportUnit unit) rd1 rd2


{-| Round down a date to the beginning of the closest interval. The resulting
date will be less than or equal to the one provided.

    fromCalendarDate 2018 May 11
        |> floor Tuesday
    -- fromCalendarDate 2018 May 8

-}
floor : Interval -> Date -> Date
floor interval (RD rd) =
    RD <| RataDie.floor (exportInterval interval) rd


{-| Create a date from a year, month, and day of the month; out-of-range day
values will be clamped.

    fromCalendarDate 2018 Sep 26

-}
fromCalendarDate : Int -> Month -> Int -> Date
fromCalendarDate y m d =
    RD <| RataDie.fromCalendarDate y (exportMonth m) d


{-| Attempt to create a date from a string in
[ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) format. Calendar dates,
week dates, and ordinal dates are all supported in extended and basic
format.

    fromIsoString "2018-09-26"
    fromIsoString "2018-W26-3"
    fromIsoString "2018-314"

The string must represent a valid date; unlike `fromCalendarDate` and
friends, any out-of-range values will fail to produce a date.

    fromIsoString "2018-02-29"
    -- Err "Invalid calendar date"

-}
fromIsoString : String -> Result String Date
fromIsoString s =
    Result.map RD <| RataDie.fromIsoString s


{-| Create a date from a year and day of the year; out-of-range day values
will be clamped.

    fromOrdinalDate 2018 314

-}
fromOrdinalDate : Int -> Int -> Date
fromOrdinalDate y od =
    RD <| RataDie.fromOrdinalDate y od


{-| Create a date from a week-numbering year, week number, and weekday;
out-of-range week values will be clamped.

    fromWeekDate 2018 26 Wed

-}
fromWeekDate : Int -> Int -> Weekday -> Date
fromWeekDate wy wn wd =
    RD <| RataDie.fromWeekDate wy wn (exportWeekday wd)


{-| -}
month : Date -> Month
month (RD rd) =
    RataDie.month rd |> importMonth


{-| -}
monthNumber : Date -> Int
monthNumber (RD rd) =
    RataDie.monthNumber rd


{-| -}
monthToNumber : Month -> Int
monthToNumber =
    exportMonth >> RataDie.monthToNumber


{-| -}
numberToMonth : Int -> Month
numberToMonth =
    RataDie.numberToMonth >> importMonth


{-| -}
numberToWeekday : Int -> Weekday
numberToWeekday =
    RataDie.numberToWeekday >> importWeekday


{-| Extracts the day of the year.
-}
ordinalDay : Date -> Int
ordinalDay (RD rd) =
    RataDie.ordinalDay rd


{-| -}
quarter : Date -> Int
quarter (RD rd) =
    RataDie.quarter rd


{-| Create a list of dates, at rounded intervals, increasing by a step value,
between two dates. The list will start on or after the first date, and end
before the second date.

    range Day 2
        (fromCalendarDate 2018 May 8)
        (fromCalendarDate 2018 May 14)

    -- [ fromCalendarDate 2018 May 8
    -- , fromCalendarDate 2018 May 10
    -- , fromCalendarDate 2018 May 12
    -- ]

-}
range : Interval -> Int -> Date -> Date -> List Date
range interval step (RD start) (RD end) =
    List.map RD <| RataDie.range (exportInterval interval) step start end


{-| -}
toCalendarDate : Date -> { year : Int, month : Month, day : Int }
toCalendarDate (RD rd) =
    let
        date =
            RataDie.toCalendarDate rd
    in
    { year = date.year
    , month = importMonth date.month
    , day = date.day
    }


{-| Convert a date to a string using a pattern as a template.

    fromCalendarDate 2007 Mar 15
        |> toFormattedString "EEEE, MMMM d, y"
    -- "Thursday, March 15, 2007"

Each alphabetic character in the pattern represents date or time information;
the number of times a character is repeated specifies the form of the name to
use (e.g. "Tue", "Tuesday") or the padding of numbers (e.g. "1", "01").
Formatting characters are escaped within single-quotes; a single-quote is
escaped as a sequence of two single-quotes, whether appearing inside or outside
an escaped sequence.

Patterns are based on Date Format Patterns in [Unicode Technical
Standard #35](http://www.unicode.org/reports/tr35/tr35-43/tr35-dates.html#Date_Format_Patterns).
Only the following subset of formatting characters are available:

    "y" -- year
    "Y" -- week-numbering year
    "Q" -- quarter
    "M" -- month
    "w" -- week number
    "d" -- day
    "D" -- ordinal day
    "E" -- day of week
    "e" -- weekday number / day of week

The non-standard pattern field "ddd" is available to indicate the day of the
month with an ordinal suffix (e.g. "1st", "15th"), as the current standard does
not include such a field.

    fromCalendarDate 2007 Mar 15
        |> toFormattedString "MMMM ddd, y"
    -- "March 15th, 2007"

-}
toFormattedString : String -> Date -> String
toFormattedString pattern (RD rd) =
    RataDie.toFormattedString pattern rd


{-| Convenience function for formatting a date in ISO 8601 extended format.

    fromCalendarDate 2007 Mar 15
        |> toIsoString
    -- "2007-03-15"

-}
toIsoString : Date -> String
toIsoString (RD rd) =
    RataDie.toIsoString rd


{-| -}
toOrdinalDate : Date -> { year : Int, ordinalDay : Int }
toOrdinalDate (RD rd) =
    RataDie.toOrdinalDate rd


{-| -}
toWeekDate : Date -> { weekYear : Int, weekNumber : Int, weekday : Weekday }
toWeekDate (RD rd) =
    let
        date =
            RataDie.toWeekDate rd
    in
    { weekYear = date.weekYear
    , weekNumber = date.weekNumber
    , weekday = importWeekday date.weekday
    }


{-| -}
weekday : Date -> Weekday
weekday (RD rd) =
    RataDie.weekday rd |> importWeekday


{-| Numbers 1–7 represent Monday–Sunday.
-}
weekdayNumber : Date -> Int
weekdayNumber (RD rd) =
    RataDie.weekdayNumber rd


{-| -}
weekdayToNumber : Weekday -> Int
weekdayToNumber =
    exportWeekday >> RataDie.weekdayToNumber


{-| -}
weekNumber : Date -> Int
weekNumber (RD rd) =
    RataDie.weekNumber rd


{-| Extracts the week-numbering year; this is not always the same as the
calendar year.
-}
weekYear : Date -> Int
weekYear (RD rd) =
    RataDie.weekYear rd


{-| -}
year : Date -> Int
year (RD rd) =
    RataDie.year rd
