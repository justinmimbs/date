module Language exposing (fr)

import Date
import Time exposing (Month(..), Weekday(..))


fr : Date.Language
fr =
    { monthName = fr_monthName
    , monthNameShort = fr_monthNameShort
    , weekdayName = fr_weekdayName
    , weekdayNameShort = fr_weekdayName >> String.left 3
    , dayWithSuffix = fr_dayWithSuffix
    }


fr_monthName : Month -> String
fr_monthName month =
    case month of
        Jan ->
            "janvier"

        Feb ->
            "février"

        Mar ->
            "mars"

        Apr ->
            "avril"

        May ->
            "mai"

        Jun ->
            "juin"

        Jul ->
            "juillet"

        Aug ->
            "août"

        Sep ->
            "septembre"

        Oct ->
            "octobre"

        Nov ->
            "novembre"

        Dec ->
            "décembre"


fr_monthNameShort : Month -> String
fr_monthNameShort month =
    case month of
        Jan ->
            "janv."

        Feb ->
            "févr."

        Mar ->
            "mars"

        Apr ->
            "avr."

        May ->
            "mai"

        Jun ->
            "juin"

        Jul ->
            "juill."

        Aug ->
            "août"

        Sep ->
            "sept."

        Oct ->
            "oct."

        Nov ->
            "nov."

        Dec ->
            "déc."


fr_weekdayName : Weekday -> String
fr_weekdayName weekday =
    case weekday of
        Mon ->
            "lundi"

        Tue ->
            "mardi"

        Wed ->
            "mercredi"

        Thu ->
            "jeudi"

        Fri ->
            "vendredi"

        Sat ->
            "samedi"

        Sun ->
            "dimanche"


fr_dayWithSuffix : Int -> String
fr_dayWithSuffix day =
    if day == 1 then
        "1er"

    else
        String.fromInt day
