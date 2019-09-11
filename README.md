# date

This Elm package provides a simple `Date` type for working with dates without times or zones.


## Installation

```sh
elm install justinmimbs/date
```


## Overview

- Get the current local date: [`today`][today]
- Get dates from `Posix` times: [`fromPosix`][fromPosix]
- Convert `Date` values both to and from:
  - [Calendar dates][fromCalendarDate] (`2018 Sep 26`)
  - [ISO week dates][fromWeekDate] (`2018 39 Wed`)
  - [Ordinal dates][fromOrdinalDate] (`2018 269`)
  - [ISO 8601 strings][fromIsoString] (`"2018-09-26"`)
  - [Rata Die][fromRataDie] (`736963`)
- Format dates for display: [`format`][format], [`formatWithLanguage`][formatWithLanguage]
- Manipulate dates: [`add`][add], [`floor`][floor], [`ceiling`][ceiling]
- Diff dates: [`diff`][diff]
- Create lists of dates: [`range`][range]
- Helpers: [`compare`][compare], [`isBetween`][isBetween], [`min`][min], [`max`][max], [`clamp`][clamp]

[today]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#today
[fromPosix]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#fromPosix
[fromCalendarDate]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#fromCalendarDate
[fromWeekDate]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#fromWeekDate
[fromOrdinalDate]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#fromOrdinalDate
[fromIsoString]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#fromIsoString
[fromRataDie]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#fromRataDie
[format]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#format
[formatWithLanguage]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#formatWithLanguage
[add]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#add
[floor]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#floor
[ceiling]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#ceiling
[diff]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#diff
[range]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#range
[compare]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#compare
[isBetween]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#isBetween
[min]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#min
[max]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#max
[clamp]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#clamp


## Examples

These examples are only meant to give a feel for the library; see [the docs][docs] for the full API.

[docs]: https://package.elm-lang.org/packages/justinmimbs/date/latest/Date


### Create a date and format it

```elm
import Date
import Time exposing (Month(..))

Date.fromCalendarDate 2018 Sep 26
    |> Date.format "EEEE, MMMM ddd, yyyy"
    == "Wednesday, September 26th, 2018"
```


### Find the next Saturday after a date

```elm
import Date exposing (Interval(..), Unit(..))
import Time exposing (Month(..))

Date.fromCalendarDate 2018 Sep 26
    |> Date.floor Saturday
    |> Date.add Weeks 1
    |> Date.toIsoString
    == "2018-09-29"
```


### List the third Thursday of the month for six months of a year

```elm
import Date exposing (Date, Interval(..), Unit(..))

start : Date
start =
    Date.fromOrdinalDate 2019 1

thirdThursday : Date -> Date
thirdThursday date =
    date |> Date.add Weeks 2 |> Date.ceiling Thursday

Date.range Month 1 start (start |> Date.add Months 6)
    |> List.map thirdThursday
    |> List.map Date.toIsoString
    == [ "2019-01-17"
       , "2019-02-21"
       , "2019-03-21"
       , "2019-04-18"
       , "2019-05-16"
       , "2019-06-20"
       ]
```
