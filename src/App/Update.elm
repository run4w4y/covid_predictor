module App.Update exposing (update)

import CovidData exposing (CountriesData)
import CovidData.Draw.LineChart exposing (drawLineChart)
import CovidData.Draw.BarChart exposing (drawBarChart)
import App.Model exposing (Model(..))
import App.Msg exposing (Msg(..))
import Html exposing (Html)
import Dict
import Date
import Array
import Time exposing (Month(..))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        ShowCountry m ->
            case m.data of
                Ok data -> 
                    case countrySvg data m.country of
                        Just svg ->
                            ( DisplayCountry data m.country svg, Cmd.none )
                        Nothing ->
                            ( CountryNotFound data m.country, Cmd.none )
                Err _ ->
                    ( HttpFailure, Cmd.none )
        ShowMap m ->
            ( Map m.data m.country, Cmd.none )
        ShowSimulation m ->
            ( Simulation m.data m.country m.params, Cmd.none )


countrySvg : CountriesData -> String -> Maybe (Html Msg)
countrySvg data country =
    case Dict.get country data of
        Just entries ->
            let
                sortedDates =
                    List.map .date entries
                        |> List.sortWith Date.compare
                        |> Array.fromList
                minDate =
                    Array.get 0 sortedDates
                        |> Maybe.withDefault (Date.fromCalendarDate 2020 Jan 1)
                maxDate =
                    Array.get (Array.length sortedDates - 1) sortedDates
                        |> Maybe.withDefault (Date.fromCalendarDate 2020 Jan 1)
                sortedValues =
                    List.map .confirmed entries
                        |> List.sort
                        |> Array.fromList
                minValue =
                    Array.get 0 sortedValues
                        |> Maybe.withDefault 0
                maxValue =
                    Array.get (Array.length sortedValues - 1) sortedValues
                        |> Maybe.withDefault 10

                params = 
                    { dataConfirmed = List.map (\x -> (x.date, toFloat x.confirmed + 1)) entries
                    , dataRecovered = List.map (\x -> (x.date, toFloat x.recovered + 1)) entries
                    , dataDeaths    = List.map (\x -> (x.date, toFloat x.deaths + 1)) entries
                    , w             = 900
                    , h             = 450
                    , padding       = 50
                    , dateFrom      = minDate
                    , dateTo        = maxDate
                    , valuesMin     = minValue
                    , valuesMax     = maxValue
                    } 

            in
            Just <| Html.div [] 
                [ Html.div [] [ drawLineChart params ]
                , Html.p [] [ Html.text "Growth:" ]
                , Html.div [] [ drawBarChart params ]
                ]
                

        Nothing ->
            Nothing