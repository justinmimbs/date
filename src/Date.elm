module Date exposing
    ( Date
    , Month, Weekday
    , today, fromPosix, fromCalendarDate, fromWeekDate, fromOrdinalDate, fromIsoString, fromRataDie
    , toIsoString, toRataDie
    , year, month, day, weekYear, weekNumber, weekday, ordinalDay, quarter, monthNumber, weekdayNumber
    , format
    , Language, formatWithLanguage
    , Unit(..), add, diff
    , Interval(..), ceiling, floor
    , range
    , compare, isBetween, min, max, clamp
    , monthToNumber, numberToMonth, weekdayToNumber, numberToWeekday
    )

{-|

@docs Date

@docs Month, Weekday


# Create

@docs today, fromPosix, fromCalendarDate, fromWeekDate, fromOrdinalDate, fromIsoString, fromRataDie


# Convert

@docs toIsoString, toRataDie


# Extract

@docs year, month, day, weekYear, weekNumber, weekday, ordinalDay, quarter, monthNumber, weekdayNumber


# Format

@docs format


## Custom Languages

@docs Language, formatWithLanguage


# Arithmetic

@docs Unit, add, diff


# Rounding

@docs Interval, ceiling, floor


# Lists

@docs range


# Ordering

@docs compare, isBetween, min, max, clamp


# Month and Weekday helpers

@docs monthToNumber, numberToMonth, weekdayToNumber, numberToWeekday

-}

import Parser exposing ((|.), (|=), Parser)
import Pattern exposing (Token(..))
import Task exposing (Task)
import Time exposing (Month(..), Posix, Weekday(..))


{-| The `Month` type used in this package is an alias of [`Month`][timemonth]
from `elm/time`. To express literal values, like `Jan`, you must import them
from `Time`.

    import Date
    import Time exposing (Month(..))

    Date.fromCalendarDate 2020 Jan 1

[timemonth]: https://package.elm-lang.org/packages/elm/time/latest/Time#Month

-}
type alias Month =
    Time.Month


{-| The `Weekday` type used in this package is an alias of [`Weekday`][timeweekday]
from `elm/time`. To express literal values, like `Mon`, you must import them
from `Time`.

    import Date
    import Time exposing (Weekday(..))

    Date.fromWeekDate 2020 1 Mon

[timeweekday]: https://package.elm-lang.org/packages/elm/time/latest/Time#Weekday

-}
type alias Weekday =
    Time.Weekday


type alias RataDie =
    Int


{-| Represents a date.
-}
type Date
    = RD RataDie


{-| [Rata Die][ratadie] is a system for assigning numbers to calendar days,
where the number 1 represents the date _1 January 0001_.

You can losslessly convert a `Date` to and from an `Int` representing the date
in Rata Die. This makes it a convenient representation for transporting dates
or using them as comparables. For all date values:

    (date |> toRataDie |> fromRataDie)
        == date

[ratadie]: https://en.wikipedia.org/wiki/Rata_Die

-}
fromRataDie : Int -> Date
fromRataDie rd =
    RD rd


{-| Convert a date to its number representation in Rata Die (see
[`fromRataDie`](#fromRataDie)). For all date values:

    (date |> toRataDie |> fromRataDie)
        == date

-}
toRataDie : Date -> Int
toRataDie (RD rd) =
    rd



-- CALCULATIONS


isLeapYear : Int -> Bool
isLeapYear y =
    modBy 4 y == 0 && modBy 100 y /= 0 || modBy 400 y == 0


daysBeforeYear : Int -> Int
daysBeforeYear y1 =
    let
        y =
            y1 - 1

        leapYears =
            floorDiv y 4 - floorDiv y 100 + floorDiv y 400
    in
    365 * y + leapYears


{-| The weekday number (1–7), beginning with Monday.
-}
weekdayNumber : Date -> Int
weekdayNumber (RD rd) =
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
    jan4 - weekdayNumber (RD jan4)


is53WeekYear : Int -> Bool
is53WeekYear y =
    let
        wdnJan1 =
            weekdayNumber (firstOfYear y)
    in
    -- any year starting on Thursday and any leap year starting on Wednesday
    wdnJan1 == 4 || (wdnJan1 == 3 && isLeapYear y)


{-| The calendar year.
-}
year : Date -> Int
year (RD rd) =
    let
        ( n400, r400 ) =
            -- 400 * 365 + 97
            divWithRemainder rd 146097

        ( n100, r100 ) =
            -- 100 * 365 + 24
            divWithRemainder r400 36524

        ( n4, r4 ) =
            -- 4 * 365 + 1
            divWithRemainder r100 1461

        ( n1, r1 ) =
            divWithRemainder r4 365

        n =
            if r1 == 0 then
                0

            else
                1
    in
    n400 * 400 + n100 * 100 + n4 * 4 + n1 + n


firstOfYear : Int -> Date
firstOfYear y =
    RD <| daysBeforeYear y + 1


firstOfMonth : Int -> Month -> Date
firstOfMonth y m =
    RD <| daysBeforeYear y + daysBeforeMonth y m + 1



-- FROM PARTS (clamps out-of-range values)


{-| Create a date from an [ordinal date][ordinaldate]: a year and day of the
year. Out-of-range day values will be clamped.

    import Date exposing (fromOrdinalDate)

    fromOrdinalDate 2018 269

[ordinaldate]: https://en.wikipedia.org/wiki/Ordinal_date

-}
fromOrdinalDate : Int -> Int -> Date
fromOrdinalDate y od =
    let
        daysInY =
            if isLeapYear y then
                366

            else
                365
    in
    RD <| daysBeforeYear y + (od |> Basics.clamp 1 daysInY)


{-| Create a date from a [calendar date][gregorian]: a year, month, and day of
the month. Out-of-range day values will be clamped.

    import Date exposing (fromCalendarDate)
    import Time exposing (Month(..))

    fromCalendarDate 2018 Sep 26

[gregorian]: https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar

-}
fromCalendarDate : Int -> Month -> Int -> Date
fromCalendarDate y m d =
    RD <| daysBeforeYear y + daysBeforeMonth y m + (d |> Basics.clamp 1 (daysInMonth y m))


{-| Create a date from an [ISO week date][weekdate]: a week-numbering year,
week number, and weekday. Out-of-range week number values will be clamped.

    import Date exposing (fromWeekDate)
    import Time exposing (Weekday(..))

    fromWeekDate 2018 39 Wed

[weekdate]: https://en.wikipedia.org/wiki/ISO_week_date

-}
fromWeekDate : Int -> Int -> Weekday -> Date
fromWeekDate wy wn wd =
    let
        weeksInWY =
            if is53WeekYear wy then
                53

            else
                52
    in
    RD <| daysBeforeWeekYear wy + ((wn |> Basics.clamp 1 weeksInWY) - 1) * 7 + (wd |> weekdayToNumber)



-- FROM NUMBERS (fails on out-of-range values)


fromOrdinalParts : Int -> Int -> Result String Date
fromOrdinalParts y od =
    if
        (od |> isBetweenInt 1 365)
            || (od == 366 && isLeapYear y)
    then
        Ok <| RD <| daysBeforeYear y + od

    else
        Err <| "Invalid ordinal date (" ++ String.fromInt y ++ ", " ++ String.fromInt od ++ ")"


fromCalendarParts : Int -> Int -> Int -> Result String Date
fromCalendarParts y mn d =
    if
        (mn |> isBetweenInt 1 12)
            && (d |> isBetweenInt 1 (daysInMonth y (mn |> numberToMonth)))
    then
        Ok <| RD <| daysBeforeYear y + daysBeforeMonth y (mn |> numberToMonth) + d

    else
        Err <| "Invalid calendar date (" ++ String.fromInt y ++ ", " ++ String.fromInt mn ++ ", " ++ String.fromInt d ++ ")"


fromWeekParts : Int -> Int -> Int -> Result String Date
fromWeekParts wy wn wdn =
    if
        (wdn |> isBetweenInt 1 7)
            && ((wn |> isBetweenInt 1 52)
                    || (wn == 53 && is53WeekYear wy)
               )
    then
        Ok <| RD <| daysBeforeWeekYear wy + (wn - 1) * 7 + wdn

    else
        Err <| "Invalid week date (" ++ String.fromInt wy ++ ", " ++ String.fromInt wn ++ ", " ++ String.fromInt wdn ++ ")"



-- TO RECORDS


{-| -}
toOrdinalDate : Date -> { year : Int, ordinalDay : Int }
toOrdinalDate (RD rd) =
    let
        y =
            year (RD rd)
    in
    { year = y
    , ordinalDay = rd - daysBeforeYear y
    }


{-| -}
toCalendarDate : Date -> { year : Int, month : Month, day : Int }
toCalendarDate (RD rd) =
    let
        date =
            RD rd |> toOrdinalDate
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
toWeekDate : Date -> { weekYear : Int, weekNumber : Int, weekday : Weekday }
toWeekDate (RD rd) =
    let
        wdn =
            weekdayNumber (RD rd)

        wy =
            -- `year <thursday of this week>`
            year (RD (rd + (4 - wdn)))

        week1Day1 =
            daysBeforeWeekYear wy + 1
    in
    { weekYear = wy
    , weekNumber = 1 + (rd - week1Day1) // 7
    , weekday = wdn |> numberToWeekday
    }



-- TO PARTS


{-| The day of the year (1–366).
-}
ordinalDay : Date -> Int
ordinalDay =
    toOrdinalDate >> .ordinalDay


{-| The month as a [`Month`](https://package.elm-lang.org/packages/elm/time/latest/Time#Month)
value (`Jan`–`Dec`).
-}
month : Date -> Month
month =
    toCalendarDate >> .month


{-| The month number (1–12).
-}
monthNumber : Date -> Int
monthNumber =
    month >> monthToNumber


{-| The quarter of the year (1–4).
-}
quarter : Date -> Int
quarter =
    month >> monthToQuarter


{-| The day of the month (1–31).
-}
day : Date -> Int
day =
    toCalendarDate >> .day


{-| The ISO week-numbering year. This is not always the same as the
calendar year.
-}
weekYear : Date -> Int
weekYear =
    toWeekDate >> .weekYear


{-| The ISO week number of the year (1–53).
-}
weekNumber : Date -> Int
weekNumber =
    toWeekDate >> .weekNumber


{-| The weekday as a [`Weekday`](https://package.elm-lang.org/packages/elm/time/latest/Time#Weekday)
value (`Mon`–`Sun`).
-}
weekday : Date -> Weekday
weekday =
    weekdayNumber >> numberToWeekday



-- quarters


monthToQuarter : Month -> Int
monthToQuarter m =
    (monthToNumber m + 2) // 3


quarterToMonth : Int -> Month
quarterToMonth q =
    q * 3 - 2 |> numberToMonth



-- TO FORMATTED STRINGS


{-| Functions to convert date information to strings in a custom language.
-}
type alias Language =
    { monthName : Month -> String
    , monthNameShort : Month -> String
    , weekdayName : Weekday -> String
    , weekdayNameShort : Weekday -> String
    , dayWithSuffix : Int -> String
    }


formatField : Language -> Char -> Int -> Date -> String
formatField language char length date =
    case char of
        'y' ->
            case length of
                2 ->
                    date |> year |> String.fromInt |> String.padLeft 2 '0' |> String.right 2

                _ ->
                    date |> year |> padSignedInt length

        'Y' ->
            case length of
                2 ->
                    date |> weekYear |> String.fromInt |> String.padLeft 2 '0' |> String.right 2

                _ ->
                    date |> weekYear |> padSignedInt length

        'Q' ->
            case length of
                1 ->
                    date |> quarter |> String.fromInt

                2 ->
                    date |> quarter |> String.fromInt

                3 ->
                    date |> quarter |> String.fromInt |> (++) "Q"

                4 ->
                    date |> quarter |> withOrdinalSuffix

                5 ->
                    date |> quarter |> String.fromInt

                _ ->
                    ""

        'M' ->
            case length of
                1 ->
                    date |> monthNumber |> String.fromInt

                2 ->
                    date |> monthNumber |> String.fromInt |> String.padLeft 2 '0'

                3 ->
                    date |> month |> language.monthNameShort

                4 ->
                    date |> month |> language.monthName

                5 ->
                    date |> month |> language.monthNameShort |> String.left 1

                _ ->
                    ""

        'w' ->
            case length of
                1 ->
                    date |> weekNumber |> String.fromInt

                2 ->
                    date |> weekNumber |> String.fromInt |> String.padLeft 2 '0'

                _ ->
                    ""

        'd' ->
            case length of
                1 ->
                    date |> day |> String.fromInt

                2 ->
                    date |> day |> String.fromInt |> String.padLeft 2 '0'

                -- non-standard
                3 ->
                    date |> day |> language.dayWithSuffix

                _ ->
                    ""

        'D' ->
            case length of
                1 ->
                    date |> ordinalDay |> String.fromInt

                2 ->
                    date |> ordinalDay |> String.fromInt |> String.padLeft 2 '0'

                3 ->
                    date |> ordinalDay |> String.fromInt |> String.padLeft 3 '0'

                _ ->
                    ""

        'E' ->
            case length of
                -- abbreviated
                1 ->
                    date |> weekday |> language.weekdayNameShort

                2 ->
                    date |> weekday |> language.weekdayNameShort

                3 ->
                    date |> weekday |> language.weekdayNameShort

                -- full
                4 ->
                    date |> weekday |> language.weekdayName

                -- narrow
                5 ->
                    date |> weekday |> language.weekdayNameShort |> String.left 1

                -- short
                6 ->
                    date |> weekday |> language.weekdayNameShort |> String.left 2

                _ ->
                    ""

        'e' ->
            case length of
                1 ->
                    date |> weekdayNumber |> String.fromInt

                2 ->
                    date |> weekdayNumber |> String.fromInt

                _ ->
                    date |> formatField language 'E' length

        _ ->
            ""


{-| Expects `tokens` list reversed for foldl.
-}
formatWithTokens : Language -> List Token -> Date -> String
formatWithTokens language tokens date =
    List.foldl
        (\token formatted ->
            case token of
                Field char length ->
                    formatField language char length date ++ formatted

                Literal str ->
                    str ++ formatted
        )
        ""
        tokens


{-| Format a date in a custom language using a string as a template.

    import Date exposing (fromOrdinalDate, formatWithLanguage)

    formatWithLanguage fr "EEEE, ddd MMMM y" (fromOrdinalDate 1970 1)
        == "jeudi, 1er janvier 1970"

    -- assuming `fr` is a custom `Date.Language`

-}
formatWithLanguage : Language -> String -> Date -> String
formatWithLanguage language pattern =
    let
        tokens =
            pattern |> Pattern.fromString |> List.reverse
    in
    formatWithTokens language tokens



-- default language


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


ordinalSuffix : Int -> String
ordinalSuffix n =
    let
        -- use 2-digit number
        nn =
            n |> modBy 100
    in
    case
        Basics.min
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


language_en : Language
language_en =
    { monthName = monthToName
    , monthNameShort = monthToName >> String.left 3
    , weekdayName = weekdayToName
    , weekdayNameShort = weekdayToName >> String.left 3
    , dayWithSuffix = withOrdinalSuffix
    }


{-| Format a date using a string as a template.

    import Date exposing (fromOrdinalDate, format)

    format "EEEE, d MMMM y" (fromOrdinalDate 1970 1)
        == "Thursday, 1 January 1970"

Alphabetic characters in the template represent date information; the number of
times a character is repeated specifies the form of a name (e.g. `"Tue"`,
`"Tuesday"`) or the padding of a number (e.g. `"1"`, `"01"`).

Alphabetic characters can be escaped within single-quotes; a single-quote can
be escaped as a sequence of two single-quotes, whether appearing inside or
outside an escaped sequence.

Templates are based on Date Format Patterns in [Unicode Technical
Standard #35][uts35]. Only the following subset of formatting characters
are available:

    "y" -- year

    "Y" -- week-numbering year

    "Q" -- quarter

    "M" -- month (number or name)

    "w" -- week number

    "d" -- day

    "D" -- ordinal day

    "E" -- weekday name

    "e" -- weekday number

[uts35]: http://www.unicode.org/reports/tr35/tr35-43/tr35-dates.html#Date_Format_Patterns

The non-standard pattern field "ddd" is available to indicate the day of the
month with an ordinal suffix (e.g. `"1st"`, `"15th"`), as the current standard
does not include such a field.

    format "MMMM ddd, y" (fromOrdinalDate 1970 1)
        == "January 1st, 1970"

-}
format : String -> Date -> String
format pattern =
    formatWithLanguage language_en pattern


{-| Convert a date to a string in ISO 8601 extended format.

    import Date exposing (fromOrdinalDate, toIsoString)

    toIsoString (fromOrdinalDate 2001 1)
        == "2001-01-01"

-}
toIsoString : Date -> String
toIsoString =
    format "yyyy-MM-dd"



-- FROM ISO 8601 STRINGS


{-| Attempt to create a date from a string in [ISO 8601][iso8601] format.
Calendar dates, week dates, and ordinal dates are all supported in extended
and basic format.

    import Date exposing (fromIsoString, fromCalendarDate, fromWeekDate, fromOrdinalDate)
    import Time exposing (Month(..), Weekday(..))

    -- calendar date
    fromIsoString "2018-09-26"
        == Ok (fromCalendarDate 2018 Sep 26)


    -- week date
    fromIsoString "2018-W39-3"
        == Ok (fromWeekDate 2018 39 Wed)


    -- ordinal date
    fromIsoString "2018-269"
        == Ok (fromOrdinalDate 2018 269)

The string must represent a valid date; unlike `fromCalendarDate` and
friends, any out-of-range values will fail to produce a date.

    fromIsoString "2018-02-29"
        == Err "Invalid calendar date (2018, 2, 29)"

[iso8601]: https://en.wikipedia.org/wiki/ISO_8601

-}
fromIsoString : String -> Result String Date
fromIsoString =
    Parser.run
        (Parser.succeed identity
            |= parser
            |. (Parser.oneOf
                    [ Parser.map Ok
                        Parser.end
                    , Parser.map (always (Err "Expected a date only, not a date and time"))
                        (Parser.chompIf ((==) 'T'))
                    , Parser.succeed (Err "Expected a date only")
                    ]
                    |> Parser.andThen resultToParser
               )
        )
        >> Result.mapError (List.map deadEndToString >> String.join "; ")


deadEndToString : Parser.DeadEnd -> String
deadEndToString { problem } =
    case problem of
        Parser.Problem message ->
            message

        _ ->
            "Expected a date in ISO 8601 format"


resultToParser : Result String a -> Parser a
resultToParser result =
    case result of
        Ok x ->
            Parser.succeed x

        Err message ->
            Parser.problem message



-- day of year


type DayOfYear
    = MonthAndDay Int Int
    | WeekAndWeekday Int Int
    | OrdinalDay Int


fromYearAndDayOfYear : ( Int, DayOfYear ) -> Result String Date
fromYearAndDayOfYear ( y, doy ) =
    case doy of
        MonthAndDay mn d ->
            fromCalendarParts y mn d

        WeekAndWeekday wn wdn ->
            fromWeekParts y wn wdn

        OrdinalDay od ->
            fromOrdinalParts y od



-- parser


parser : Parser Date
parser =
    Parser.succeed Tuple.pair
        |= int4
        |= dayOfYear
        |> Parser.andThen
            (fromYearAndDayOfYear >> resultToParser)


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
        |. Parser.oneOf
            [ Parser.chompIf (\c -> c == '-')
            , Parser.succeed ()
            ]
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



-- ARITHMETIC


{-| -}
type Unit
    = Years
    | Months
    | Weeks
    | Days


{-| Get a past or future date by adding a number of units to a date.

    import Date exposing (Unit(..), add, fromCalendarDate)
    import Time exposing (Month(..))

    add Weeks -2 (fromCalendarDate 2018 Sep 26)
        == fromCalendarDate 2018 Sep 12

When adding `Years` or `Months`, day values are clamped to the end of the
month if necessary.

    add Months 1 (fromCalendarDate 2000 Jan 31)
        == fromCalendarDate 2000 Feb 29

-}
add : Unit -> Int -> Date -> Date
add unit n (RD rd) =
    case unit of
        Years ->
            RD rd |> add Months (12 * n)

        Months ->
            let
                date =
                    RD rd |> toCalendarDate

                wholeMonths =
                    12 * (date.year - 1) + (monthToNumber date.month - 1) + n

                y =
                    floorDiv wholeMonths 12 + 1

                m =
                    (wholeMonths |> modBy 12) + 1 |> numberToMonth
            in
            RD <| daysBeforeYear y + daysBeforeMonth y m + Basics.min date.day (daysInMonth y m)

        Weeks ->
            RD <| rd + 7 * n

        Days ->
            RD <| rd + n


{-| The number of whole months between date and 0001-01-01 plus fraction
representing the current month. Only used for diffing months.
-}
toMonths : RataDie -> Float
toMonths rd =
    let
        date =
            RD rd |> toCalendarDate

        wholeMonths =
            12 * (date.year - 1) + (monthToNumber date.month - 1)
    in
    toFloat wholeMonths + toFloat date.day / 100


{-| Get the difference, as a number of whole units, between two dates.

    import Date exposing (Unit(..), diff, fromCalendarDate)
    import Time exposing (Month(..))

    diff Months
        (fromCalendarDate 2020 Jan 2)
        (fromCalendarDate 2020 Apr 1)
        == 2

-}
diff : Unit -> Date -> Date -> Int
diff unit (RD rd1) (RD rd2) =
    case unit of
        Years ->
            (toMonths rd2 - toMonths rd1 |> truncate) // 12

        Months ->
            toMonths rd2 - toMonths rd1 |> truncate

        Weeks ->
            (rd2 - rd1) // 7

        Days ->
            rd2 - rd1



-- ROUNDING


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


daysSincePreviousWeekday : Weekday -> Date -> Int
daysSincePreviousWeekday wd date =
    (weekdayNumber date + 7 - weekdayToNumber wd) |> modBy 7


{-| Round down a date to the beginning of the closest interval. The resulting
date will be less than or equal to the one provided.

    import Date exposing (Interval(..), floor, fromCalendarDate)
    import Time exposing (Month(..))

    floor Tuesday (fromCalendarDate 2018 May 11)
        == fromCalendarDate 2018 May 8

-}
floor : Interval -> Date -> Date
floor interval ((RD rd) as date) =
    case interval of
        Year ->
            firstOfYear (year date)

        Quarter ->
            firstOfMonth (year date) (quarter date |> quarterToMonth)

        Month ->
            firstOfMonth (year date) (month date)

        Week ->
            RD <| rd - daysSincePreviousWeekday Mon date

        Monday ->
            RD <| rd - daysSincePreviousWeekday Mon date

        Tuesday ->
            RD <| rd - daysSincePreviousWeekday Tue date

        Wednesday ->
            RD <| rd - daysSincePreviousWeekday Wed date

        Thursday ->
            RD <| rd - daysSincePreviousWeekday Thu date

        Friday ->
            RD <| rd - daysSincePreviousWeekday Fri date

        Saturday ->
            RD <| rd - daysSincePreviousWeekday Sat date

        Sunday ->
            RD <| rd - daysSincePreviousWeekday Sun date

        Day ->
            date


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

    import Date exposing (Interval(..), ceiling, fromCalendarDate)
    import Time exposing (Month(..))

    ceiling Tuesday (fromCalendarDate 2018 May 11)
        == fromCalendarDate 2018 May 15

-}
ceiling : Interval -> Date -> Date
ceiling interval date =
    let
        floored =
            date |> floor interval
    in
    if date == floored then
        date

    else
        let
            ( n, unit ) =
                interval |> intervalToUnits
        in
        floored |> add unit n



-- LISTS


{-| Create a list of dates, at rounded intervals, increasing by a step value,
between two dates. The list will start on or after the first date, and end
before the second date.

    import Date exposing (Interval(..), range, fromCalendarDate)
    import Time exposing (Month(..))

    start = fromCalendarDate 2018 May 8
    until = fromCalendarDate 2018 May 14

    range Day 2 start until
        == [ fromCalendarDate 2018 May 8
           , fromCalendarDate 2018 May 10
           , fromCalendarDate 2018 May 12
           ]

-}
range : Interval -> Int -> Date -> Date -> List Date
range interval step (RD start) (RD until) =
    let
        ( n, unit ) =
            interval |> intervalToUnits

        (RD first) =
            RD start |> ceiling interval
    in
    if first < until then
        rangeHelp unit (Basics.max 1 step * n) until [] first

    else
        []


rangeHelp : Unit -> Int -> RataDie -> List Date -> RataDie -> List Date
rangeHelp unit step until revList current =
    if current < until then
        let
            (RD next) =
                RD current |> add unit step
        in
        rangeHelp unit step until (RD current :: revList) next

    else
        List.reverse revList



-- POSIX


{-| Create a date from a time [`Zone`][zone] and a [`Posix`][posix] time. This
conversion loses the time information associated with the `Posix` value.

    import Date exposing (fromCalendarDate, fromPosix)
    import Time exposing (millisToPosix, utc, Month(..))

    fromPosix utc (millisToPosix 0)
        == fromCalendarDate 1970 Jan 1

[zone]: https://package.elm-lang.org/packages/elm/time/latest/Time#Zone
[posix]: https://package.elm-lang.org/packages/elm/time/latest/Time#Posix

-}
fromPosix : Time.Zone -> Posix -> Date
fromPosix zone posix =
    fromCalendarDate
        (posix |> Time.toYear zone)
        (posix |> Time.toMonth zone)
        (posix |> Time.toDay zone)


{-| Get the current local date. See [this page][calendarexample] for a full example.

[calendarexample]: https://github.com/justinmimbs/date/blob/master/examples/Calendar.elm

-}
today : Task Never Date
today =
    Task.map2 fromPosix Time.here Time.now



-- ORDERING


{-| Compare two dates. This can be used as the compare function for
`List.sortWith`.

    import Date exposing (fromOrdinalDate, compare)

    compare (fromOrdinalDate 1970 1) (fromOrdinalDate 2038 1)
        == LT

-}
compare : Date -> Date -> Order
compare (RD a) (RD b) =
    Basics.compare a b


{-| Test if a date is within a range, inclusive of the range values.

    import Date exposing (fromOrdinalDate, isBetween)

    minimum = fromOrdinalDate 1970 1
    maximum = fromOrdinalDate 2038 1

    isBetween minimum maximum (fromOrdinalDate 1969 201)
        == False

-}
isBetween : Date -> Date -> Date -> Bool
isBetween (RD a) (RD b) (RD x) =
    isBetweenInt a b x


{-| Find the lesser of two dates.

    import Date exposing (fromOrdinalDate, min)

    min (fromOrdinalDate 1970 1) (fromOrdinalDate 2038 1)
        == (fromOrdinalDate 1970 1)

-}
min : Date -> Date -> Date
min ((RD a) as dateA) ((RD b) as dateB) =
    if a < b then
        dateA

    else
        dateB


{-| Find the greater of two dates.

    import Date exposing (fromOrdinalDate, max)

    max (fromOrdinalDate 1970 1) (fromOrdinalDate 2038 1)
        == (fromOrdinalDate 2038 1)

-}
max : Date -> Date -> Date
max ((RD a) as dateA) ((RD b) as dateB) =
    if a < b then
        dateB

    else
        dateA


{-| Clamp a date within a range.

    import Date exposing (fromOrdinalDate, clamp)

    minimum = fromOrdinalDate 1970 1
    maximum = fromOrdinalDate 2038 1

    clamp minimum maximum (fromOrdinalDate 1969 201)
        == fromOrdinalDate 1970 1

-}
clamp : Date -> Date -> Date -> Date
clamp ((RD a) as dateA) ((RD b) as dateB) ((RD x) as dateX) =
    if x < a then
        dateA

    else if b < x then
        dateB

    else
        dateX



-- NUMBERS OF DAYS


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



-- MONTH AND WEEKDAY NUMBERS


{-| Maps `Jan`–`Dec` to 1–12.
-}
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


{-| Maps 1–12 to `Jan`–`Dec`.
-}
numberToMonth : Int -> Month
numberToMonth mn =
    case Basics.max 1 mn of
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


{-| Maps `Mon`–`Sun` to 1-7.
-}
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


{-| Maps 1-7 to `Mon`–`Sun`.
-}
numberToWeekday : Int -> Weekday
numberToWeekday wdn =
    case Basics.max 1 wdn of
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



-- HELPERS


padSignedInt : Int -> Int -> String
padSignedInt length int =
    (if int < 0 then
        "-"

     else
        ""
    )
        ++ (abs int |> String.fromInt |> String.padLeft length '0')


floorDiv : Int -> Int -> Int
floorDiv a b =
    Basics.floor (toFloat a / toFloat b)


{-| integer division, returning (Quotient, Remainder)
-}
divWithRemainder : Int -> Int -> ( Int, Int )
divWithRemainder a b =
    ( floorDiv a b, a |> modBy b )


isBetweenInt : Int -> Int -> Int -> Bool
isBetweenInt a b x =
    a <= x && x <= b
