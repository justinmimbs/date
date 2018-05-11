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
        , firstOfMonth
        , firstOfWeekYear
        , firstOfYear
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
        , weekNumber
        , weekYear
        , weekday
        , weekdayNumber
        , weekdayToNumber
        , year
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


firstOfWeekYear : Int -> RataDie
firstOfWeekYear wy =
    daysBeforeWeekYear wy + 1



-- extract


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



-- constructors, clamping


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


fromCalendarDate : Int -> Month -> Int -> RataDie
fromCalendarDate y m d =
    daysBeforeYear y + daysBeforeMonth y m + (d |> clamp 1 (daysInMonth y m))


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



-- ISO 8601


isoDateRegex : Regex
isoDateRegex =
    let
        year_ =
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
            "(\\-)?W(\\d{2})(?:\\5(\\d))?"

        ord =
            --     ddd
            --     8
            "\\-?(\\d{3})"
    in
    ("^" ++ year_ ++ "(?:" ++ cal ++ "|" ++ week ++ "|" ++ ord ++ ")?$")
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


fromIsoStringMatches : List (Maybe String) -> Result String RataDie
fromIsoStringMatches =
    let
        toInt : Maybe String -> Int
        toInt =
            Maybe.andThen String.toInt >> Maybe.withDefault 1
    in
    \matches ->
        case matches of
            [ Just yyyy, _, mn, d, _, wn, wdn, od ] ->
                let
                    y =
                        yyyy |> String.toInt |> Maybe.withDefault 1
                in
                case ( mn, wn ) of
                    ( Just _, Nothing ) ->
                        fromCalendarParts y (mn |> toInt) (d |> toInt)

                    ( Nothing, Just _ ) ->
                        fromWeekParts y (wn |> toInt) (wdn |> toInt)

                    _ ->
                        fromOrdinalParts y (od |> toInt)

            _ ->
                Err "Unexpected results from isoDateRegex"


fromIsoString : String -> Result String RataDie
fromIsoString =
    Regex.find isoDateRegex
        >> List.head
        >> Result.fromMaybe "String is not in IS0 8601 date format"
        >> Result.andThen (.submatches >> fromIsoStringMatches)



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


weekNumber : RataDie -> Int
weekNumber =
    toWeekDate >> .weekNumber


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


{-| Matches a series of pattern characters, or a single-quoted string (which
may contain '' inside, representing an escaped single-quote).
-}
patternMatches : Regex
patternMatches =
    Regex.fromString "([yYQMwdDEe])\\1*|'(?:[^']|'')*?'(?!')" |> Maybe.withDefault Regex.never


escapedSingleQuote : Regex
escapedSingleQuote =
    Regex.fromString "''" |> Maybe.withDefault Regex.never


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
format rd match =
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
                    rd |> year |> String.fromInt |> String.padLeft 2 '0' |> String.right 2

                _ ->
                    rd |> year |> String.fromInt |> String.padLeft length '0'

        "Y" ->
            case length of
                2 ->
                    rd |> weekYear |> String.fromInt |> String.padLeft 2 '0' |> String.right 2

                _ ->
                    rd |> weekYear |> String.fromInt |> String.padLeft length '0'

        "Q" ->
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

        "M" ->
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

        "w" ->
            case length of
                1 ->
                    rd |> weekNumber |> String.fromInt

                2 ->
                    rd |> weekNumber |> String.fromInt |> String.padLeft 2 '0'

                _ ->
                    ""

        "d" ->
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

        "D" ->
            case length of
                1 ->
                    rd |> ordinalDay |> String.fromInt

                2 ->
                    rd |> ordinalDay |> String.fromInt |> String.padLeft 2 '0'

                3 ->
                    rd |> ordinalDay |> String.fromInt |> String.padLeft 3 '0'

                _ ->
                    ""

        "E" ->
            case length |> toNameForm of
                "abbreviated" ->
                    rd |> weekday |> weekdayToName |> String.left 3

                "full" ->
                    rd |> weekday |> weekdayToName

                "narrow" ->
                    rd |> weekday |> weekdayToName |> String.left 1

                "short" ->
                    rd |> weekday |> weekdayToName |> String.left 2

                _ ->
                    ""

        "e" ->
            case length of
                1 ->
                    rd |> weekdayNumber |> String.fromInt

                2 ->
                    rd |> weekdayNumber |> String.fromInt

                _ ->
                    format rd (String.toUpper match)

        "'" ->
            if match == "''" then
                "'"
            else
                String.slice 1 -1 match |> Regex.replace escapedSingleQuote (\_ -> "'")

        _ ->
            ""


toFormattedString : String -> RataDie -> String
toFormattedString pattern rd =
    Regex.replace patternMatches (.match >> format rd) pattern


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


type Unit
    = Years
    | Months
    | Weeks
    | Days


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
