module Date.RataDie
    exposing
        ( Interval(..)
        , Month(..)
        , RataDie
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
        , toWeekDate
        , today
        , weekNumber
        , weekYear
        , weekday
        , weekdayNumber
        , weekdayToNumber
        , year
        )

{-| [Rata Die](https://en.wikipedia.org/wiki/Rata_Die) is a system for
assigning numbers to calendar days, using a base date of _1 January 0001_.

This module exposes the same functions as the `Date` module, but it uses raw
`Int` values as date representations.

This may be useful if you need dates as comparables. Otherwise, the `Date`
module offers an opaque type for better type-safety.

@docs RataDie, Month, Weekday


## Constructors

@docs fromCalendarDate, fromOrdinalDate, fromWeekDate, today


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

-}

import Parser exposing ((|.), (|=), Parser)
import Pattern exposing (Token(..))
import Task exposing (Task)
import Time


{-| -}
type alias RataDie =
    Int


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



-- calculations


isLeapYear : Int -> Bool
isLeapYear y =
    modBy 4 y == 0 && modBy 100 y /= 0 || modBy 400 y == 0


daysBeforeYear : Int -> Int
daysBeforeYear y1 =
    let
        y =
            y1 - 1

        leapYears =
            (y // 4) - (y // 100) + (y // 400)
    in
    365 * y + leapYears


{-| Numbers 1–7 represent Monday–Sunday.
-}
weekdayNumber : RataDie -> Int
weekdayNumber rd =
    case rd |> modBy 7 of
        0 ->
            7

        n ->
            n


daysBeforeWeekYear : Int -> Int
daysBeforeWeekYear y =
    let
        jan4 =
            daysBeforeYear y + 4
    in
    jan4 - weekdayNumber jan4


is53WeekYear : Int -> Bool
is53WeekYear y =
    let
        wdnJan1 =
            daysBeforeYear y + 1 |> weekdayNumber
    in
    -- any year starting on Thursday and any leap year starting on Wednesday
    wdnJan1 == 4 || (wdnJan1 == 3 && isLeapYear y)



-- create


firstOfYear : Int -> RataDie
firstOfYear y =
    daysBeforeYear y + 1


firstOfMonth : Int -> Month -> RataDie
firstOfMonth y m =
    daysBeforeYear y + daysBeforeMonth y m + 1



-- extract


{-| -}
year : RataDie -> Int
year rd =
    let
        ( n400, r400 ) =
            -- 400 * 365 + 97
            divideInt rd 146097

        ( n100, r100 ) =
            -- 100 * 365 + 24
            divideInt r400 36524

        ( n4, r4 ) =
            -- 4 * 365 + 1
            divideInt r100 1461

        ( n1, r1 ) =
            divideInt r4 365

        n =
            if r1 == 0 then
                0

            else
                1
    in
    n400 * 400 + n100 * 100 + n4 * 4 + n1 + n


{-| integer division, returning (Quotient, Remainder)
-}
divideInt : Int -> Int -> ( Int, Int )
divideInt a b =
    ( a // b, a |> remainderBy b )



-- constructors, clamping


{-| Create a date from a year and day of the year; out-of-range day values
will be clamped.

    fromOrdinalDate 2018 314

-}
fromOrdinalDate : Int -> Int -> RataDie
fromOrdinalDate y od =
    let
        daysInY =
            if isLeapYear y then
                366

            else
                365
    in
    daysBeforeYear y + (od |> clamp 1 daysInY)


{-| Create a date from a year, month, and day of the month; out-of-range day
values will be clamped.

    fromCalendarDate 2018 Sep 26

-}
fromCalendarDate : Int -> Month -> Int -> RataDie
fromCalendarDate y m d =
    daysBeforeYear y + daysBeforeMonth y m + (d |> clamp 1 (daysInMonth y m))


{-| Create a date from a week-numbering year, week number, and weekday;
out-of-range week values will be clamped.

    fromWeekDate 2018 26 Wed

-}
fromWeekDate : Int -> Int -> Weekday -> RataDie
fromWeekDate wy wn wd =
    let
        weeksInWY =
            if is53WeekYear wy then
                53

            else
                52
    in
    daysBeforeWeekYear wy + ((wn |> clamp 1 weeksInWY) - 1) * 7 + (wd |> weekdayToNumber)



-- constructors, strict


fromOrdinalParts : Int -> Int -> Result String RataDie
fromOrdinalParts y od =
    if
        (od |> isBetween 1 365)
            || (od == 366 && isLeapYear y)
    then
        Ok <| daysBeforeYear y + od

    else
        Err <| "Invalid ordinal date (" ++ String.fromInt y ++ ", " ++ String.fromInt od ++ ")"


fromCalendarParts : Int -> Int -> Int -> Result String RataDie
fromCalendarParts y mn d =
    if
        (mn |> isBetween 1 12)
            && (d |> isBetween 1 (daysInMonth y (mn |> numberToMonth)))
    then
        Ok <| daysBeforeYear y + daysBeforeMonth y (mn |> numberToMonth) + d

    else
        Err <| "Invalid calendar date (" ++ String.fromInt y ++ ", " ++ String.fromInt mn ++ ", " ++ String.fromInt d ++ ")"


fromWeekParts : Int -> Int -> Int -> Result String RataDie
fromWeekParts wy wn wdn =
    if
        (wdn |> isBetween 1 7)
            && ((wn |> isBetween 1 52)
                    || (wn == 53 && is53WeekYear wy)
               )
    then
        Ok <| daysBeforeWeekYear wy + (wn - 1) * 7 + wdn

    else
        Err <| "Invalid week date (" ++ String.fromInt wy ++ ", " ++ String.fromInt wn ++ ", " ++ String.fromInt wdn ++ ")"


isBetween : Int -> Int -> Int -> Bool
isBetween a b x =
    a <= x && x <= b



-- ISO 8601


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
fromIsoString : String -> Result String RataDie
fromIsoString =
    Parser.run yearAndDay
        >> Result.mapError (\_ -> "String is not in IS0 8601 date format")
        >> Result.andThen fromYearAndDay


type DayOfYear
    = MonthAndDay Int Int
    | WeekAndWeekday Int Int
    | OrdinalDay Int


fromYearAndDay : ( Int, DayOfYear ) -> Result String RataDie
fromYearAndDay ( y, doy ) =
    case doy of
        MonthAndDay mn d ->
            fromCalendarParts y mn d

        WeekAndWeekday wn wdn ->
            fromWeekParts y wn wdn

        OrdinalDay od ->
            fromOrdinalParts y od



-- parser


yearAndDay : Parser ( Int, DayOfYear )
yearAndDay =
    Parser.succeed Tuple.pair
        |= int4
        |= dayOfYear
        |. Parser.end


dayOfYear : Parser DayOfYear
dayOfYear =
    Parser.oneOf
        [ Parser.succeed identity
            -- extended format
            |. Parser.token "-"
            |= Parser.oneOf
                [ Parser.backtrackable
                    (Parser.map OrdinalDay
                        int3
                        |> Parser.andThen Parser.commit
                    )
                , Parser.succeed MonthAndDay
                    |= int2
                    |= Parser.oneOf
                        [ Parser.succeed identity
                            |. Parser.token "-"
                            |= int2
                        , Parser.succeed 1
                        ]
                , Parser.succeed WeekAndWeekday
                    |. Parser.token "W"
                    |= int2
                    |= Parser.oneOf
                        [ Parser.succeed identity
                            |. Parser.token "-"
                            |= int1
                        , Parser.succeed 1
                        ]
                ]

        -- basic format
        , Parser.backtrackable
            (Parser.succeed MonthAndDay
                |= int2
                |= Parser.oneOf
                    [ int2
                    , Parser.succeed 1
                    ]
                |> Parser.andThen Parser.commit
            )
        , Parser.map OrdinalDay
            int3
        , Parser.succeed WeekAndWeekday
            |. Parser.token "W"
            |= int2
            |= Parser.oneOf
                [ int1
                , Parser.succeed 1
                ]
        , Parser.succeed
            (OrdinalDay 1)
        ]


int4 : Parser Int
int4 =
    Parser.succeed ()
        |. Parser.chompIf Char.isDigit
        |. Parser.chompIf Char.isDigit
        |. Parser.chompIf Char.isDigit
        |. Parser.chompIf Char.isDigit
        |> Parser.mapChompedString
            (\str _ -> String.toInt str |> Maybe.withDefault 0)


int3 : Parser Int
int3 =
    Parser.succeed ()
        |. Parser.chompIf Char.isDigit
        |. Parser.chompIf Char.isDigit
        |. Parser.chompIf Char.isDigit
        |> Parser.mapChompedString
            (\str _ -> String.toInt str |> Maybe.withDefault 0)


int2 : Parser Int
int2 =
    Parser.succeed ()
        |. Parser.chompIf Char.isDigit
        |. Parser.chompIf Char.isDigit
        |> Parser.mapChompedString
            (\str _ -> String.toInt str |> Maybe.withDefault 0)


int1 : Parser Int
int1 =
    Parser.chompIf Char.isDigit
        |> Parser.mapChompedString
            (\str _ -> String.toInt str |> Maybe.withDefault 0)



-- to


{-| -}
toOrdinalDate : RataDie -> { year : Int, ordinalDay : Int }
toOrdinalDate rd =
    let
        y =
            year rd
    in
    { year = y
    , ordinalDay = rd - daysBeforeYear y
    }


{-| -}
toCalendarDate : RataDie -> { year : Int, month : Month, day : Int }
toCalendarDate rd =
    let
        date =
            rd |> toOrdinalDate
    in
    toCalendarDateHelp date.year Jan date.ordinalDay


toCalendarDateHelp : Int -> Month -> Int -> { year : Int, month : Month, day : Int }
toCalendarDateHelp y m d =
    let
        monthDays =
            daysInMonth y m

        mn =
            m |> monthToNumber
    in
    if mn < 12 && d > monthDays then
        toCalendarDateHelp y (mn + 1 |> numberToMonth) (d - monthDays)

    else
        { year = y
        , month = m
        , day = d
        }


{-| -}
toWeekDate : RataDie -> { weekYear : Int, weekNumber : Int, weekday : Weekday }
toWeekDate rd =
    let
        wdn =
            weekdayNumber rd

        wy =
            -- `year <thursday of this week>`
            year (rd + (4 - wdn))

        week1Day1 =
            daysBeforeWeekYear wy + 1
    in
    { weekYear = wy
    , weekNumber = 1 + (rd - week1Day1) // 7
    , weekday = wdn |> numberToWeekday
    }



-- lookups


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


daysBeforeMonth : Int -> Month -> Int
daysBeforeMonth y m =
    let
        leapDays =
            if isLeapYear y then
                1

            else
                0
    in
    case m of
        Jan ->
            0

        Feb ->
            31

        Mar ->
            59 + leapDays

        Apr ->
            90 + leapDays

        May ->
            120 + leapDays

        Jun ->
            151 + leapDays

        Jul ->
            181 + leapDays

        Aug ->
            212 + leapDays

        Sep ->
            243 + leapDays

        Oct ->
            273 + leapDays

        Nov ->
            304 + leapDays

        Dec ->
            334 + leapDays



-- conversions


{-| -}
monthToNumber : Month -> Int
monthToNumber m =
    case m of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


{-| -}
numberToMonth : Int -> Month
numberToMonth mn =
    case max 1 mn of
        1 ->
            Jan

        2 ->
            Feb

        3 ->
            Mar

        4 ->
            Apr

        5 ->
            May

        6 ->
            Jun

        7 ->
            Jul

        8 ->
            Aug

        9 ->
            Sep

        10 ->
            Oct

        11 ->
            Nov

        _ ->
            Dec


{-| -}
weekdayToNumber : Weekday -> Int
weekdayToNumber wd =
    case wd of
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


{-| -}
numberToWeekday : Int -> Weekday
numberToWeekday wdn =
    case max 1 wdn of
        1 ->
            Mon

        2 ->
            Tue

        3 ->
            Wed

        4 ->
            Thu

        5 ->
            Fri

        6 ->
            Sat

        _ ->
            Sun


monthToQuarter : Month -> Int
monthToQuarter m =
    (monthToNumber m + 2) // 3


quarterToMonth : Int -> Month
quarterToMonth q =
    q * 3 - 2 |> numberToMonth



-- extractions (convenience)


{-| Extracts the day of the year.
-}
ordinalDay : RataDie -> Int
ordinalDay =
    toOrdinalDate >> .ordinalDay


{-| -}
month : RataDie -> Month
month =
    toCalendarDate >> .month


{-| -}
monthNumber : RataDie -> Int
monthNumber =
    month >> monthToNumber


{-| -}
quarter : RataDie -> Int
quarter =
    month >> monthToQuarter


{-| Extracts the day of the month.
-}
day : RataDie -> Int
day =
    toCalendarDate >> .day


{-| Extracts the week-numbering year; this is not always the same as the
calendar year.
-}
weekYear : RataDie -> Int
weekYear =
    toWeekDate >> .weekYear


{-| -}
weekNumber : RataDie -> Int
weekNumber =
    toWeekDate >> .weekNumber


{-| -}
weekday : RataDie -> Weekday
weekday =
    weekdayNumber >> numberToWeekday



-- formatting (based on Date Format Patterns in Unicode Technical Standard #35)


ordinalSuffix : Int -> String
ordinalSuffix n =
    let
        -- use 2-digit number
        nn =
            n |> modBy 100
    in
    case
        min
            (if nn < 20 then
                nn

             else
                nn |> modBy 10
            )
            4
    of
        1 ->
            "st"

        2 ->
            "nd"

        3 ->
            "rd"

        _ ->
            "th"


withOrdinalSuffix : Int -> String
withOrdinalSuffix n =
    String.fromInt n ++ ordinalSuffix n


formatField : Char -> Int -> RataDie -> String
formatField char length rd =
    case char of
        'y' ->
            case length of
                2 ->
                    rd |> year |> String.fromInt |> String.padLeft 2 '0' |> String.right 2

                _ ->
                    rd |> year |> String.fromInt |> String.padLeft length '0'

        'Y' ->
            case length of
                2 ->
                    rd |> weekYear |> String.fromInt |> String.padLeft 2 '0' |> String.right 2

                _ ->
                    rd |> weekYear |> String.fromInt |> String.padLeft length '0'

        'Q' ->
            case length of
                1 ->
                    rd |> quarter |> String.fromInt

                2 ->
                    rd |> quarter |> String.fromInt

                3 ->
                    rd |> quarter |> String.fromInt |> (++) "Q"

                4 ->
                    rd |> quarter |> withOrdinalSuffix

                5 ->
                    rd |> quarter |> String.fromInt

                _ ->
                    ""

        'M' ->
            case length of
                1 ->
                    rd |> monthNumber |> String.fromInt

                2 ->
                    rd |> monthNumber |> String.fromInt |> String.padLeft 2 '0'

                3 ->
                    rd |> month |> monthToName |> String.left 3

                4 ->
                    rd |> month |> monthToName

                5 ->
                    rd |> month |> monthToName |> String.left 1

                _ ->
                    ""

        'w' ->
            case length of
                1 ->
                    rd |> weekNumber |> String.fromInt

                2 ->
                    rd |> weekNumber |> String.fromInt |> String.padLeft 2 '0'

                _ ->
                    ""

        'd' ->
            case length of
                1 ->
                    rd |> day |> String.fromInt

                2 ->
                    rd |> day |> String.fromInt |> String.padLeft 2 '0'

                -- non-standard
                3 ->
                    rd |> day |> withOrdinalSuffix

                _ ->
                    ""

        'D' ->
            case length of
                1 ->
                    rd |> ordinalDay |> String.fromInt

                2 ->
                    rd |> ordinalDay |> String.fromInt |> String.padLeft 2 '0'

                3 ->
                    rd |> ordinalDay |> String.fromInt |> String.padLeft 3 '0'

                _ ->
                    ""

        'E' ->
            case length of
                -- abbreviated
                1 ->
                    rd |> weekday |> weekdayToName |> String.left 3

                2 ->
                    rd |> weekday |> weekdayToName |> String.left 3

                3 ->
                    rd |> weekday |> weekdayToName |> String.left 3

                -- full
                4 ->
                    rd |> weekday |> weekdayToName

                -- narrow
                5 ->
                    rd |> weekday |> weekdayToName |> String.left 1

                -- short
                6 ->
                    rd |> weekday |> weekdayToName |> String.left 2

                _ ->
                    ""

        'e' ->
            case length of
                1 ->
                    rd |> weekdayNumber |> String.fromInt

                2 ->
                    rd |> weekdayNumber |> String.fromInt

                _ ->
                    rd |> formatField 'E' length

        _ ->
            ""


{-| Expects `tokens` list reversed for foldl.
-}
formatWithTokens : List Token -> RataDie -> String
formatWithTokens tokens rd =
    List.foldl
        (\token formatted ->
            case token of
                Field char length ->
                    formatField char length rd ++ formatted

                Literal str ->
                    str ++ formatted
        )
        ""
        tokens


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
toFormattedString : String -> RataDie -> String
toFormattedString pattern =
    let
        tokens =
            pattern |> Pattern.fromString |> List.reverse
    in
    formatWithTokens tokens


{-| Convenience function for formatting a date in ISO 8601 extended format.

    fromCalendarDate 2007 Mar 15
        |> toIsoString
    -- "2007-03-15"

-}
toIsoString : RataDie -> String
toIsoString =
    toFormattedString "yyyy-MM-dd"



-- lookups (names)


monthToName : Month -> String
monthToName m =
    case m of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


weekdayToName : Weekday -> String
weekdayToName wd =
    case wd of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"



-- arithmetic


{-| -}
type Unit
    = Years
    | Months
    | Weeks
    | Days


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
add : Unit -> Int -> RataDie -> RataDie
add unit n rd =
    case unit of
        Years ->
            rd |> add Months (12 * n)

        Months ->
            let
                date =
                    rd |> toCalendarDate

                wholeMonths =
                    12 * (date.year - 1) + monthToNumber date.month - 1 + n

                y =
                    wholeMonths // 12 + 1

                m =
                    (wholeMonths |> modBy 12) + 1 |> numberToMonth
            in
            daysBeforeYear y + daysBeforeMonth y m + min date.day (daysInMonth y m)

        Weeks ->
            rd + 7 * n

        Days ->
            rd + n


{-| The number of whole months between date and 0001-01-01 plus fraction
representing the current month. Only used for diffing months.
-}
toMonths : RataDie -> Float
toMonths rd =
    let
        date =
            rd |> toCalendarDate

        wholeMonths =
            12 * (date.year - 1) + monthToNumber date.month - 1
    in
    toFloat wholeMonths + toFloat date.day / 100


{-| Find the difference, as a number of some units, between two dates.

    diff Months
        (fromCalendarDate 2007 Mar 15)
        (fromCalendarDate 2007 Sep 1)
    -- 5

-}
diff : Unit -> RataDie -> RataDie -> Int
diff unit rd1 rd2 =
    case unit of
        Years ->
            (toMonths rd2 - toMonths rd1 |> truncate) // 12

        Months ->
            toMonths rd2 - toMonths rd1 |> truncate

        Weeks ->
            (rd2 - rd1) // 7

        Days ->
            rd2 - rd1



-- intervals


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


daysSincePreviousWeekday : Weekday -> RataDie -> Int
daysSincePreviousWeekday wd rd =
    (weekdayNumber rd + 7 - weekdayToNumber wd) |> modBy 7


{-| Round down a date to the beginning of the closest interval. The resulting
date will be less than or equal to the one provided.

    fromCalendarDate 2018 May 11
        |> floor Tuesday
    -- fromCalendarDate 2018 May 8

-}
floor : Interval -> RataDie -> RataDie
floor interval rd =
    case interval of
        Year ->
            firstOfYear (year rd)

        Quarter ->
            let
                date =
                    rd |> toCalendarDate
            in
            firstOfMonth date.year (date.month |> monthToQuarter |> quarterToMonth)

        Month ->
            let
                date =
                    rd |> toCalendarDate
            in
            firstOfMonth date.year date.month

        Week ->
            rd - daysSincePreviousWeekday Mon rd

        Monday ->
            rd - daysSincePreviousWeekday Mon rd

        Tuesday ->
            rd - daysSincePreviousWeekday Tue rd

        Wednesday ->
            rd - daysSincePreviousWeekday Wed rd

        Thursday ->
            rd - daysSincePreviousWeekday Thu rd

        Friday ->
            rd - daysSincePreviousWeekday Fri rd

        Saturday ->
            rd - daysSincePreviousWeekday Sat rd

        Sunday ->
            rd - daysSincePreviousWeekday Sun rd

        Day ->
            rd


intervalToUnits : Interval -> ( Int, Unit )
intervalToUnits interval =
    case interval of
        Year ->
            ( 1, Years )

        Quarter ->
            ( 3, Months )

        Month ->
            ( 1, Months )

        Day ->
            ( 1, Days )

        week ->
            ( 1, Weeks )


{-| Round up a date to the beginning of the closest interval. The resulting
date will be greater than or equal to the one provided.

    fromCalendarDate 2018 May 11
        |> ceiling Tuesday
    -- fromCalendarDate 2018 May 15

-}
ceiling : Interval -> RataDie -> RataDie
ceiling interval rd =
    let
        floored =
            rd |> floor interval
    in
    if rd == floored then
        rd

    else
        let
            ( n, unit ) =
                interval |> intervalToUnits
        in
        floored |> add unit n


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
range : Interval -> Int -> RataDie -> RataDie -> List RataDie
range interval step start end =
    let
        ( n, unit ) =
            interval |> intervalToUnits

        first =
            start |> ceiling interval
    in
    if first < end then
        rangeHelp unit (max 1 step * n) end [] first

    else
        []


rangeHelp : Unit -> Int -> RataDie -> List RataDie -> RataDie -> List RataDie
rangeHelp unit step end revList rd =
    if rd < end then
        rangeHelp unit step end (rd :: revList) (rd |> add unit step)

    else
        List.reverse revList



-- today


{-| Get the current local date.
-}
today : Task Never RataDie
today =
    Task.map2
        (\currentTime currentOffset ->
            fromCalendarDate
                (currentTime |> Time.toYear currentOffset)
                (currentTime |> Time.toMonth currentOffset |> fromTimeMonth)
                (currentTime |> Time.toDay currentOffset)
        )
        Time.now
        Time.here


fromTimeMonth : Time.Month -> Month
fromTimeMonth m =
    case m of
        Time.Jan ->
            Jan

        Time.Feb ->
            Feb

        Time.Mar ->
            Mar

        Time.Apr ->
            Apr

        Time.May ->
            May

        Time.Jun ->
            Jun

        Time.Jul ->
            Jul

        Time.Aug ->
            Aug

        Time.Sep ->
            Sep

        Time.Oct ->
            Oct

        Time.Nov ->
            Nov

        Time.Dec ->
            Dec
