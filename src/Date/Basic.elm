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
        , firstOfMonth
        , firstOfWeekYear
        , firstOfYear
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

import Date.RataDie as RataDie exposing (RataDie)


type Date
    = RD Int


toRataDie : Date -> RataDie
toRataDie (RD rd) =
    rd


fromRataDie : RataDie -> Date
fromRataDie rd =
    RD rd



--


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


type Unit
    = Years
    | Months
    | Weeks
    | Days


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


add : Unit -> Int -> Date -> Date
add unit n (RD rd) =
    RD <| RataDie.add (exportUnit unit) n rd


ceiling : Interval -> Date -> Date
ceiling interval (RD rd) =
    RD <| RataDie.ceiling (exportInterval interval) rd


day : Date -> Int
day (RD rd) =
    RataDie.day rd


diff : Unit -> Date -> Date -> Int
diff unit (RD rd1) (RD rd2) =
    RataDie.diff (exportUnit unit) rd1 rd2


firstOfMonth : Int -> Month -> Date
firstOfMonth y m =
    RD <| RataDie.firstOfMonth y (exportMonth m)


firstOfWeekYear : Int -> Date
firstOfWeekYear wy =
    RD <| RataDie.firstOfWeekYear wy


firstOfYear : Int -> Date
firstOfYear y =
    RD <| RataDie.firstOfYear y


floor : Interval -> Date -> Date
floor interval (RD rd) =
    RD <| RataDie.floor (exportInterval interval) rd


fromCalendarDate : Int -> Month -> Int -> Date
fromCalendarDate y m d =
    RD <| RataDie.fromCalendarDate y (exportMonth m) d


fromIsoString : String -> Result String Date
fromIsoString s =
    Result.map RD <| RataDie.fromIsoString s


fromOrdinalDate : Int -> Int -> Date
fromOrdinalDate y od =
    RD <| RataDie.fromOrdinalDate y od


fromWeekDate : Int -> Int -> Weekday -> Date
fromWeekDate wy wn wd =
    RD <| RataDie.fromWeekDate wy wn (exportWeekday wd)


month : Date -> Month
month (RD rd) =
    RataDie.month rd |> importMonth


monthNumber : Date -> Int
monthNumber (RD rd) =
    RataDie.monthNumber rd


monthToNumber : Month -> Int
monthToNumber =
    exportMonth >> RataDie.monthToNumber


numberToMonth : Int -> Month
numberToMonth =
    RataDie.numberToMonth >> importMonth


numberToWeekday : Int -> Weekday
numberToWeekday =
    RataDie.numberToWeekday >> importWeekday


ordinalDay : Date -> Int
ordinalDay (RD rd) =
    RataDie.ordinalDay rd


quarter : Date -> Int
quarter (RD rd) =
    RataDie.quarter rd


range : Interval -> Int -> Date -> Date -> List Date
range interval step (RD start) (RD end) =
    List.map RD <| RataDie.range (exportInterval interval) step start end


toCalendarDate : Date -> { year : Int, month : Month, day : Int }
toCalendarDate (RD rd) =
    let
        { year, month, day } =
            RataDie.toCalendarDate rd
    in
    { year = year
    , month = importMonth month
    , day = day
    }


toFormattedString : String -> Date -> String
toFormattedString pattern (RD rd) =
    RataDie.toFormattedString pattern rd


toIsoString : Date -> String
toIsoString (RD rd) =
    RataDie.toIsoString rd


toOrdinalDate : Date -> { year : Int, ordinalDay : Int }
toOrdinalDate (RD rd) =
    RataDie.toOrdinalDate rd


toWeekDate : Date -> { weekYear : Int, weekNumber : Int, weekday : Weekday }
toWeekDate (RD rd) =
    let
        { weekYear, weekNumber, weekday } =
            RataDie.toWeekDate rd
    in
    { weekYear = weekYear
    , weekNumber = weekNumber
    , weekday = importWeekday weekday
    }


weekday : Date -> Weekday
weekday (RD rd) =
    RataDie.weekday rd |> importWeekday


weekdayNumber : Date -> Int
weekdayNumber (RD rd) =
    RataDie.weekdayNumber rd


weekdayToNumber : Weekday -> Int
weekdayToNumber =
    exportWeekday >> RataDie.weekdayToNumber


weekNumber : Date -> Int
weekNumber (RD rd) =
    RataDie.weekNumber rd


weekYear : Date -> Int
weekYear (RD rd) =
    RataDie.weekYear rd


year : Date -> Int
year (RD rd) =
    RataDie.year rd
