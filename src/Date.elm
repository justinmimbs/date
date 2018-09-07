module Date exposing
    ( Date
    , today, fromCalendarDate, fromOrdinalDate, fromWeekDate
    , format
    , fromIsoString, toIsoString
    , Unit(..), add, diff
    , Interval(..), ceiling, floor
    , range
    , year, quarter, month, monthNumber, ordinalDay, day, weekYear, weekNumber, weekday, weekdayNumber
    , Month, Weekday
    , monthToNumber, numberToMonth, weekdayToNumber, numberToWeekday
    , toRataDie, fromRataDie
    )

{-|

@docs Date

@docs today, fromCalendarDate, fromOrdinalDate, fromWeekDate


## Formatting

@docs format


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


## Month and Weekday

@docs Month, Weekday

@docs monthToNumber, numberToMonth, weekdayToNumber, numberToWeekday


## Rata Die

Convert a `Date` to and from a raw `Int` representing the date in
[Rata Die](https://en.wikipedia.org/wiki/Rata_Die).

@docs toRataDie, fromRataDie

-}

import Date.RataDie as RataDie exposing (RataDie)
import Task exposing (Task)
import Time exposing (Month(..), Weekday(..))


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


{-| The `Month` type used in this package is an alias of [`Time.Month`](https://package.elm-lang.org/packages/elm/time/latest/Time#Month)
from `elm/time`. So if you need to express `Month` values, like `Jan`, then
you'll need to import them from `Time`.

    import Date exposing (Date)
    import Time exposing (Month(..))

    Date.fromCalendarDate 2020 Jan 1

-}
type alias Month =
    Time.Month


{-| The `Weekday` type used in this package is an alias of [`Time.Weekday`](https://package.elm-lang.org/packages/elm/time/latest/Time#Weekday)
from `elm/time`. So if you need to express `Weekday` values, like `Mon`, then
you'll need to import them from `Time`.

    import Date exposing (Date)
    import Time exposing (Weekday(..))

    Date.fromWeekDate 2020 1 Mon

-}
type alias Weekday =
    Time.Weekday


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


{-| Create a date from a year, month, and day of the month. Out-of-range day
values will be clamped.

    fromCalendarDate 2018 Sep 26

-}
fromCalendarDate : Int -> Month -> Int -> Date
fromCalendarDate y m d =
    RD <| RataDie.fromCalendarDate y m d


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


{-| Create a date from a year and day of the year. Out-of-range day values
will be clamped.

    fromOrdinalDate 2018 314

-}
fromOrdinalDate : Int -> Int -> Date
fromOrdinalDate y od =
    RD <| RataDie.fromOrdinalDate y od


{-| Create a date from a week-numbering year, week number, and weekday.
Out-of-range week values will be clamped.

    fromWeekDate 2018 26 Wed

-}
fromWeekDate : Int -> Int -> Weekday -> Date
fromWeekDate wy wn wd =
    RD <| RataDie.fromWeekDate wy wn wd


{-| -}
month : Date -> Month
month (RD rd) =
    RataDie.month rd


{-| -}
monthNumber : Date -> Int
monthNumber (RD rd) =
    RataDie.monthNumber rd


{-| -}
monthToNumber : Month -> Int
monthToNumber =
    RataDie.monthToNumber


{-| -}
numberToMonth : Int -> Month
numberToMonth =
    RataDie.numberToMonth


{-| -}
numberToWeekday : Int -> Weekday
numberToWeekday =
    RataDie.numberToWeekday


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


{-| Convert a date to a string using a pattern as a template.

    fromCalendarDate 2007 Mar 15
        |> format "EEEE, MMMM d, y"
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
        |> format "MMMM ddd, y"
    -- "March 15th, 2007"

-}
format : String -> Date -> String
format pattern (RD rd) =
    RataDie.format pattern rd


{-| Convenience function for formatting a date in ISO 8601 extended format.

    fromCalendarDate 2007 Mar 15
        |> toIsoString
    -- "2007-03-15"

-}
toIsoString : Date -> String
toIsoString (RD rd) =
    RataDie.toIsoString rd


{-| -}
weekday : Date -> Weekday
weekday (RD rd) =
    RataDie.weekday rd


{-| Numbers 1–7 represent Monday–Sunday.
-}
weekdayNumber : Date -> Int
weekdayNumber (RD rd) =
    RataDie.weekdayNumber rd


{-| -}
weekdayToNumber : Weekday -> Int
weekdayToNumber =
    RataDie.weekdayToNumber


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



-- today


{-| Get the current local date.
-}
today : Task Never Date
today =
    Task.map RD RataDie.today
