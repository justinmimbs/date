module Date.RataDie
    exposing
        ( Month(..)
        , RataDie
        , Weekday(..)
        , fromCalendarDate
        , fromOrdinalDate
        , fromWeekDate
        , toCalendarDate
        , toOrdinalDate
        , toWeekDate
        )


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
    monthNumber >> toFloat >> (\n -> n / 3) >> ceiling


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
