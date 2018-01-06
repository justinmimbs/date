module Date.RataDie
    exposing
        ( RataDie
        , fromCalendarDate
        , fromOrdinalDate
        , fromWeekDate
        , toCalendarDate
        , toOrdinalDate
        , toWeekDate
        )


type alias RataDie =
    Int



-- from


fromOrdinalDate : Int -> Int -> RataDie
fromOrdinalDate y od =
    daysBeforeYear y + od


fromCalendarDate : Int -> Int -> Int -> RataDie
fromCalendarDate y m d =
    daysBeforeYear y + daysBeforeMonth y m + d


fromWeekDate : Int -> Int -> Int -> RataDie
fromWeekDate wy w wd =
    daysBeforeWeekYear wy + (w - 1) * 7 + wd



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


toCalendarDate : RataDie -> { year : Int, month : Int, day : Int }
toCalendarDate rd =
    let
        date =
            rd |> toOrdinalDate
    in
    toCalendarDateHelp date.year 1 date.ordinalDay


toCalendarDateHelp : Int -> Int -> Int -> { year : Int, month : Int, day : Int }
toCalendarDateHelp y m d =
    let
        monthDays =
            daysInMonth y m
    in
    if m < 12 && d > monthDays then
        toCalendarDateHelp y (m + 1) (d - monthDays)
    else
        { year = y
        , month = m
        , day = d
        }


toWeekDate : RataDie -> { weekYear : Int, week : Int, weekday : Int }
toWeekDate rd =
    let
        wd =
            weekday rd

        wy =
            -- `year thursday`
            year (rd + (4 - wd))

        week1Day1 =
            daysBeforeWeekYear wy + 1
    in
    { weekYear = wy
    , week = 1 + (rd - week1Day1) // 7
    , weekday = wd
    }



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


weekday : RataDie -> Int
weekday rd =
    case rd % 7 of
        0 ->
            7

        n ->
            n



-- helpers


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
    jan4 - weekday jan4


daysBeforeMonth : Int -> Int -> Int
daysBeforeMonth y m =
    let
        leapDays =
            if isLeapYear y then
                1
            else
                0
    in
    case m % 12 of
        1 ->
            0

        2 ->
            31

        3 ->
            59 + leapDays

        4 ->
            90 + leapDays

        5 ->
            120 + leapDays

        6 ->
            151 + leapDays

        7 ->
            181 + leapDays

        8 ->
            212 + leapDays

        9 ->
            243 + leapDays

        10 ->
            273 + leapDays

        11 ->
            304 + leapDays

        _ ->
            334 + leapDays


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


{-| integer division, returning (Quotient, Remainder)
-}
divideInt : Int -> Int -> ( Int, Int )
divideInt a b =
    ( a // b, rem a b )
