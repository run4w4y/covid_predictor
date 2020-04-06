module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import CovidData exposing (..)
import CovidData.Encode exposing (..)
import CovidData.Decode exposing (..)
import CovidData.Draw exposing (drawLineChart)
import Json.Encode as E
import Http
import Dict
import List
import String
import Array
import Platform.Cmd as C
import Date exposing (Date)
import Time exposing (Month(..))


-- Main

main =
    Browser.element 
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- Model

type Model 
    = HttpFailure
    | CountryNotFound CountriesData
    | Loading
    | Success CountriesData (Html Msg)

init : () -> (Model, Cmd Msg)
init _ = 
    ( Loading
    , loadData
    )

loadData : Cmd Msg
loadData =
    Http.get 
        { url = "https://pomber.github.io/covid19/timeseries.json"
        , expect = Http.expectJson identity dataDecoder
        }
        |> C.map (\x -> Msg x "Afghanistan")


-- Update

type alias Msg = 
    { data    : Result Http.Error CountriesData
    , country : String
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg.data of 
        Ok data -> 
            case Dict.get msg.country data of
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
                    ( Success data 
                        <| drawLineChart 
                            { data      = List.map (\x -> (x.date, toFloat x.confirmed)) entries
                            , w         = 900
                            , h         = 450
                            , padding   = 30
                            , dateFrom  = minDate
                            , dateTo    = maxDate
                            , valuesMin = minValue
                            , valuesMax = maxValue
                            } 
                    , Cmd.none
                    )
                Nothing ->
                    (CountryNotFound data, Cmd.none)
        Err _ ->
            (HttpFailure, Cmd.none)


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- View

view : Model -> Html Msg
view model =
    case model of
        HttpFailure ->
            pre [] [ text "something went wrong with fetching data over http" ]
        
        Loading ->
            pre [] [ text "loading" ]

        CountryNotFound data ->
            pre [] [ text "country could not be found" ]
                |> defaultLayout data

        Success data svgChart ->
            defaultLayout data svgChart


defaultLayout : CountriesData -> Html Msg -> Html Msg
defaultLayout data elem = 
    div [] 
        [ pre [] [ text <| "Total confirmed: " ++ (String.fromInt <| total .confirmed data) ] 
        , pre [] [ text <| "Total deaths: " ++  (String.fromInt <| total .deaths data) ]
        , pre [] [ text <| "Total recovered: " ++ (String.fromInt <| total .recovered data) ]
        , select [ onInput (\x -> Msg (Ok data) x) ]  
            (Dict.keys data |> List.map (\x -> option [ value x ] [ text x ]))
        , elem
        ]

total : (DataEntry -> Int) -> CountriesData -> Int
total f data = 
    Dict.values data
        |> List.map (totalCountry f)
        |> List.sum 

totalCountry : (DataEntry -> Int) -> List DataEntry -> Int
totalCountry f data =
    let 
        data_ = Array.fromList data
    in
    case Array.get (Array.length data_ - 1) data_ of
        Just entry ->
            f entry
        Nothing ->
            0
