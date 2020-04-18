module Main exposing (main)

import App.Model exposing (Model(..))
import App.Update exposing (update)
import App.Msg exposing (Msg(..))
import App.View.Default
import App.View.Loading
import App.View.Error
import App.View.Country
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Dict
import Element exposing (..)
import Browser
import CovidData exposing (..)
import CovidData.Decode exposing (..)
import Http
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
        |> C.map (\x -> ShowCountry { data = x, country = "Afghanistan" })



-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- View

view : Model -> Html Msg
view model =
    layout 
        [ height fill
        , width fill
        , scrollbars
        ] <| case model of
            HttpFailure ->
                App.View.Error.view 
                    "something went wrong with fetching data over http"
            
            Loading ->
                App.View.Loading.view

            CountryNotFound data country ->
                App.View.Default.view 
                    { leftSide = html <| Html.pre [] [ Html.text "couldnt find country" ]
                    , rightSide = countrySelect data
                    , data = data
                    , country = country
                    }

            DisplayCountry data country svgChart ->
                App.View.Country.view 
                    { leftSide = svgChart
                    , rightSide = countrySelect data
                    , data = data
                    , country = country
                    }
            
            Map data country -> 
                App.View.Default.view 
                    { leftSide = 
                        Html.iframe 
                            [ Html.Attributes.src "https://ourworldindata.org/grapher/total-cases-covid-19?tab=map"
                            , Html.Attributes.style "height" "100%"
                            ] 
                            []
                            |> html
                            |> el [ width fill, height fill ] 
                    , rightSide = none
                    , data = data
                    , country = country
                    }
            
            Simulation data country _ ->
                App.View.Default.view 
                    { leftSide = none
                    , rightSide = none
                    , data = data
                    , country = country
                    }

countrySelect : CountriesData -> Element Msg
countrySelect data = 
    [ App.View.Default.makeHeader [ text "Select country" ] |> el [ width shrink ]
    , html <| Html.select [ Html.Events.onInput (\x -> ShowCountry { data = Ok data, country = x }) ]  
        (Dict.keys data 
            |> List.map (\x -> Html.option [ Html.Attributes.value x ] [ Html.text x ]))
    ] |> paragraph []
