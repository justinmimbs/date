module Calendar exposing (main)

import Browser
import Date exposing (Date, Interval(..), Unit(..))
import Html exposing (Html)
import Html.Attributes
import Task exposing (Task)
import Time exposing (Month(..))


main : Program () Model Msg
main =
    Browser.document
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    Date


type Msg
    = ReceiveDate Date


init : ( Model, Cmd Msg )
init =
    ( Date.fromCalendarDate 2019 Jan 1
    , Date.today |> Task.perform ReceiveDate
    )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update (ReceiveDate today) _ =
    ( today
    , Cmd.none
    )



-- helpers


monthDates : Int -> Month -> List Date
monthDates year month =
    let
        start =
            Date.fromCalendarDate year month 1
                |> Date.floor Monday

        until =
            start |> Date.add Days 42
    in
    Date.range Day 1 start until


groupsOf : Int -> List a -> List (List a)
groupsOf n list =
    if List.isEmpty list then
        []

    else
        List.take n list :: groupsOf n (List.drop n list)



-- view


view : Model -> Browser.Document Msg
view date =
    Browser.Document
        "Calendar"
        [ Html.div
            [ Html.Attributes.style "padding" "2em"
            , Html.Attributes.style "font-family" "Helvetica, Arial, san-serif"
            , Html.Attributes.style "font-size" "16px"
            ]
            [ Html.h2
                [ Html.Attributes.style "font-size" "16px"
                , Html.Attributes.style "margin" "0"
                , Html.Attributes.style "padding" "0 0.5em 2em"
                ]
                [ Html.text (date |> Date.format "MMMM yyyy")
                ]
            , viewMonthTable date
            ]
        ]


weekdayHeader : Html a
weekdayHeader =
    Html.thead
        []
        [ Html.tr
            []
            ([ "Mo", "Tu", "We", "Th", "Fr", "Sa", "Su" ]
                |> List.map
                    (\str ->
                        Html.th
                            [ Html.Attributes.style "padding" "0.5em"
                            , Html.Attributes.style "font-weight" "normal"
                            , Html.Attributes.style "font-style" "italic"
                            , Html.Attributes.style "color" "gray"
                            ]
                            [ Html.text str
                            ]
                    )
            )
        ]


viewMonthTable : Date -> Html a
viewMonthTable target =
    let
        weeks =
            monthDates (Date.year target) (Date.month target)
                |> groupsOf 7
    in
    Html.table
        [ Html.Attributes.style "border-collapse" "collapse"
        , Html.Attributes.style "text-align" "right"
        ]
        [ weekdayHeader
        , Html.tbody
            []
            (weeks
                |> List.map
                    (\weekdates ->
                        Html.tr
                            []
                            (weekdates
                                |> List.map
                                    (\date ->
                                        let
                                            color =
                                                if Date.month date == Date.month target then
                                                    "black"

                                                else
                                                    "lightgray"

                                            background =
                                                if date == target then
                                                    "lightskyblue"

                                                else
                                                    "transparent"
                                        in
                                        Html.td
                                            [ Html.Attributes.style "padding" "0.5em"
                                            , Html.Attributes.style "background" background
                                            , Html.Attributes.style "color" color
                                            ]
                                            [ Html.text (Date.day date |> String.fromInt)
                                            ]
                                    )
                            )
                    )
            )
        ]
