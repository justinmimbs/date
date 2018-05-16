# date

This Elm package provides a simple `Date` type for working with dates without times or zones.


## Installation

```sh
elm install justinmimbs/date
```


## Examples

```elm
import Date exposing (Date, Month(..), Weekday(..), Unit(..), Interval(..))


Date.fromCalendarDate 2018 Sep 26
    |> Date.floor Monday
    |> Date.add Weeks -2
    |> Date.toFormattedString "EEEE, MMMM ddd, yyyy"

-- "Monday, September 10th, 2018"


Date.fromWeekDate 2018 39 Wed
    |> Date.toRataDie

-- 736963


Date.fromRataDie 736963
    |> Date.toWeekDate

-- { weekYear = 2018, weekNumber = 39, weekday = Wed }


Date.range
    Wednesday
    1
    (Date.fromCalendarDate 2018 Sep 1)
    (Date.fromCalendarDate 2018 Oct 1)
    |> List.map
        Date.toIsoString

-- [ "2018-09-05", "2018-09-12", "2018-09-19", "2018-09-26" ]
```
