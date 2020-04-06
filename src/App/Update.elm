module App.Update exposing (update)

import CovidData exposing (CountriesData)
import CovidData.Draw exposing (drawLineChart)
import App.Model exposing (Model(..))
import App.Msg exposing (Msg)
import Html exposing (Html)
import Dict
import Date
import Array
import Time exposing (Month(..))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg.data of 
        Ok data -> 
            case countrySvg data msg.country of
                Just svg ->
                    ( Success data msg.country svg, Cmd.none )
                Nothing ->
                    (CountryNotFound data msg.country, Cmd.none)
        Err _ ->
            (HttpFailure, Cmd.none)

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
            in
            Just <| drawLineChart 
                { data      = List.map (\x -> (x.date, toFloat x.confirmed)) entries
                , w         = 900
                , h         = 450
                , padding   = 50
                , dateFrom  = minDate
                , dateTo    = maxDate
                , valuesMin = minValue
                , valuesMax = maxValue
                } 

        Nothing ->
            Nothing