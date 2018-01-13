module Date.RataDie
    exposing
        ( Interval(..)
        , Month(..)
        , RataDie
        , Unit(..)
        , Weekday(..)
        , add
        , ceiling
        , diff
        , floor
        , fromCalendarDate
        , fromIsoString
        , fromOrdinalDate
        , fromWeekDate
        , range
        , toCalendarDate
        , toFormattedString
        , toOrdinalDate
        , toWeekDate
        )

import Regex exposing (Regex)


type alias RataDie =
    Int


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
    y % 4 == 0 && y % 100 /= 0 || y % 400 == 0


daysBeforeYear : Int -> RataDie
daysBeforeYear y1 =
    let
        y =
            y1 - 1

        leapYears =
            (y // 4) - (y // 100) + (y // 400)
    in
    365 * y + leapYears


daysBeforeWeekYear : Int -> RataDie
daysBeforeWeekYear y =
    let
        jan4 =
            daysBeforeYear y + 4
    in
    jan4 - weekdayNumber jan4



-- extractions


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


weekdayNumber : RataDie -> Int
weekdayNumber rd =
    case rd % 7 of
        0 ->
            7

        n ->
            n


{-| integer division, returning (Quotient, Remainder)
-}
divideInt : Int -> Int -> ( Int, Int )
divideInt a b =
    ( a // b, rem a b )



-- from


fromOrdinalDate : Int -> Int -> RataDie
fromOrdinalDate y od =
    daysBeforeYear y + od


fromCalendarDate : Int -> Month -> Int -> RataDie
fromCalendarDate y m d =
    daysBeforeYear y + daysBeforeMonth y m + d


fromWeekDate : Int -> Int -> Weekday -> RataDie
fromWeekDate wy w wd =
    daysBeforeWeekYear wy + (w - 1) * 7 + (wd |> weekdayToNumber)



-- ISO 8601


isoDateRegex : Regex
isoDateRegex =
    let
        year =
            -- yyyy
            -- 1
            "(\\d{4})"

        cal =
            --       mm            dd
            -- 2     3             4
            "(\\-)?(\\d{2})(?:\\2(\\d{2}))?"

        week =
            --        ww            d
            -- 5      6             7
            "(\\-)?W(\\d{2})(?:\\5([1-7]))?"

        ord =
            --     ddd
            --     8
            "\\-?(\\d{3})"
    in
    Regex.regex <| "^" ++ year ++ "(?:" ++ cal ++ "|" ++ week ++ "|" ++ ord ++ ")?$"


fromIsoStringMatches : List (Maybe String) -> Maybe RataDie
fromIsoStringMatches =
    let
        toInt : Maybe String -> Int
        toInt =
            Maybe.andThen (String.toInt >> Result.toMaybe) >> Maybe.withDefault 1
    in
    \matches ->
        case matches of
            [ Just yyyy, _, mm, dd, _, ww, d, ddd ] ->
                Just <|
                    let
                        y =
                            yyyy |> String.toInt |> Result.withDefault 1
                    in
                    case ( mm, ww ) of
                        ( Just _, Nothing ) ->
                            fromCalendarDate y (mm |> toInt |> numberToMonth) (dd |> toInt)

                        ( Nothing, Just _ ) ->
                            fromWeekDate y (ww |> toInt) (d |> toInt |> numberToWeekday)

                        _ ->
                            fromOrdinalDate y (ddd |> toInt)

            _ ->
                Nothing


fromIsoString : String -> Maybe RataDie
fromIsoString =
    Regex.find (Regex.AtMost 1) isoDateRegex
        >> List.head
        >> Maybe.andThen (.submatches >> fromIsoStringMatches)



-- to


toOrdinalDate : RataDie -> { year : Int, ordinalDay : Int }
toOrdinalDate rd =
    let
        y =
            year rd
    in
    { year = y
    , ordinalDay = rd - daysBeforeYear y
    }


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


toWeekDate : RataDie -> { weekYear : Int, week : Int, weekday : Weekday }
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
    , week = 1 + (rd - week1Day1) // 7
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


numberToMonth : Int -> Month
numberToMonth mn =
    case mn of
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


numberToWeekday : Int -> Weekday
numberToWeekday wdn =
    case wdn of
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


ordinalDay : RataDie -> Int
ordinalDay =
    toOrdinalDate >> .ordinalDay


month : RataDie -> Month
month =
    toCalendarDate >> .month


monthNumber : RataDie -> Int
monthNumber =
    month >> monthToNumber


quarter : RataDie -> Int
quarter =
    month >> monthToQuarter


day : RataDie -> Int
day =
    toCalendarDate >> .day


weekYear : RataDie -> Int
weekYear =
    toWeekDate >> .weekYear


week : RataDie -> Int
week =
    toWeekDate >> .week


weekday : RataDie -> Weekday
weekday =
    toWeekDate >> .weekday



-- formatting (based on Date Format Patterns in Unicode Technical Standard #35)


ordinalSuffix : Int -> String
ordinalSuffix n =
    let
        -- use 2-digit number
        nn =
            n % 100
    in
    case
        min
            (if nn < 20 then
                nn
             else
                nn % 10
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
    toString n ++ ordinalSuffix n


{-| Matches a series of pattern characters, or a single-quoted string (which
may contain '' inside, representing an escaped single-quote).
-}
patternMatches : Regex
patternMatches =
    Regex.regex "([yYQMwdDEe])\\1*|'(?:[^']|'')*?'(?!')"


toNameForm : Int -> String
toNameForm length =
    case length of
        1 ->
            "abbreviated"

        2 ->
            "abbreviated"

        3 ->
            "abbreviated"

        4 ->
            "full"

        5 ->
            "narrow"

        6 ->
            "short"

        _ ->
            "invalid"


format : RataDie -> String -> String
format date match =
    let
        char =
            String.left 1 match

        length =
            String.length match
    in
    case char of
        "y" ->
            case length of
                2 ->
                    date |> year |> toString |> String.padLeft 2 '0' |> String.right 2

                _ ->
                    date |> year |> toString |> String.padLeft length '0'

        "Y" ->
            case length of
                2 ->
                    date |> weekYear |> toString |> String.padLeft 2 '0' |> String.right 2

                _ ->
                    date |> weekYear |> toString |> String.padLeft length '0'

        "Q" ->
            case length of
                1 ->
                    date |> quarter |> toString

                2 ->
                    date |> quarter |> toString

                3 ->
                    date |> quarter |> toString |> (++) "Q"

                4 ->
                    date |> quarter |> withOrdinalSuffix

                5 ->
                    date |> quarter |> toString

                _ ->
                    ""

        "M" ->
            case length of
                1 ->
                    date |> monthNumber |> toString

                2 ->
                    date |> monthNumber |> toString |> String.padLeft 2 '0'

                3 ->
                    date |> month |> monthToName |> String.left 3

                4 ->
                    date |> month |> monthToName

                5 ->
                    date |> month |> monthToName |> String.left 1

                _ ->
                    ""

        "w" ->
            case length of
                1 ->
                    date |> week |> toString

                2 ->
                    date |> week |> toString |> String.padLeft 2 '0'

                _ ->
                    ""

        "d" ->
            case length of
                1 ->
                    date |> day |> toString

                2 ->
                    date |> day |> toString |> String.padLeft 2 '0'

                -- non-standard
                3 ->
                    date |> day |> withOrdinalSuffix

                _ ->
                    ""

        "D" ->
            case length of
                1 ->
                    date |> ordinalDay |> toString

                2 ->
                    date |> ordinalDay |> toString |> String.padLeft 2 '0'

                3 ->
                    date |> ordinalDay |> toString |> String.padLeft 3 '0'

                _ ->
                    ""

        "E" ->
            case length |> toNameForm of
                "abbreviated" ->
                    date |> weekday |> weekdayToName |> String.left 3

                "full" ->
                    date |> weekday |> weekdayToName

                "narrow" ->
                    date |> weekday |> weekdayToName |> String.left 1

                "short" ->
                    date |> weekday |> weekdayToName |> String.left 2

                _ ->
                    ""

        "e" ->
            case length of
                1 ->
                    date |> weekdayNumber |> toString

                2 ->
                    date |> weekdayNumber |> toString

                _ ->
                    format date (String.toUpper match)

        "'" ->
            if match == "''" then
                "'"
            else
                String.slice 1 -1 match |> Regex.replace Regex.All (Regex.regex "''") (\_ -> "'")

        _ ->
            ""


toFormattedString : String -> RataDie -> String
toFormattedString pattern date =
    Regex.replace Regex.All patternMatches (.match >> format date) pattern



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
weekdayToName d =
    case d of
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


type Unit
    = Years
    | Months
    | Weeks
    | Days


add : Unit -> Int -> RataDie -> RataDie
add unit n date =
    case unit of
        Years ->
            date |> add Months (12 * n)

        Months ->
            let
                { year, month, day } =
                    date |> toCalendarDate

                wholeMonths =
                    12 * (year - 1) + monthToNumber month - 1 + n

                y =
                    wholeMonths // 12 + 1

                m =
                    wholeMonths % 12 + 1 |> numberToMonth
            in
            fromCalendarDate y m (day |> Basics.min (daysInMonth y m))

        Weeks ->
            date + 7 * n

        Days ->
            date + n


{-| The number of whole months between date and 0001-01-01 plus fraction
representing the current month. Only used for diffing months.
-}
toMonths : RataDie -> Float
toMonths date =
    let
        { year, month, day } =
            date |> toCalendarDate

        wholeMonths =
            12 * (year - 1) + monthToNumber month - 1
    in
    toFloat wholeMonths + toFloat day / 100


diff : Unit -> RataDie -> RataDie -> Int
diff unit date1 date2 =
    case unit of
        Years ->
            (toMonths date2 - toMonths date1 |> truncate) // 12

        Months ->
            toMonths date2 - toMonths date1 |> truncate

        Weeks ->
            (date2 - date1) // 7

        Days ->
            date2 - date1



-- intervals


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
daysSincePreviousWeekday wd date =
    (weekdayNumber date + 7 - weekdayToNumber wd) % 7


floor : Interval -> RataDie -> RataDie
floor interval date =
    let
        { year, month, day } =
            date |> toCalendarDate
    in
    case interval of
        Year ->
            fromCalendarDate year Jan 1

        Quarter ->
            fromCalendarDate year (month |> monthToQuarter |> quarterToMonth) 1

        Month ->
            fromCalendarDate year month 1

        Week ->
            date - daysSincePreviousWeekday Mon date

        Monday ->
            date - daysSincePreviousWeekday Mon date

        Tuesday ->
            date - daysSincePreviousWeekday Tue date

        Wednesday ->
            date - daysSincePreviousWeekday Wed date

        Thursday ->
            date - daysSincePreviousWeekday Thu date

        Friday ->
            date - daysSincePreviousWeekday Fri date

        Saturday ->
            date - daysSincePreviousWeekday Sat date

        Sunday ->
            date - daysSincePreviousWeekday Sun date

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


ceiling : Interval -> RataDie -> RataDie
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


range : Interval -> Int -> RataDie -> RataDie -> List RataDie
range interval step start end =
    let
        stepBack =
            max 1 step |> negate

        ( n, unit ) =
            interval |> intervalToUnits
    in
    rangeHelp [] unit (n * stepBack) start (end |> add unit (n * stepBack) |> ceiling interval)


rangeHelp : List RataDie -> Unit -> Int -> RataDie -> RataDie -> List RataDie
rangeHelp result unit step start date =
    if date < start then
        result
    else
        rangeHelp (date :: result) unit step start (date |> add unit step)
