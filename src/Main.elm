module Main exposing (main)

import App.Model exposing (Model(..))
import App.Update exposing (update)
import App.Msg exposing (Msg)
import App.View.Default
import App.View.Loading
import App.View.Error
import Html exposing (Html)
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
        |> C.map (\x -> Msg x "Afghanistan")



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
                    { leftSide = Html.pre [] [ Html.text "couldnt find country" ]
                    , data = data
                    , country = country
                    }

            Success data country svgChart ->
                App.View.Default.view 
                    { leftSide = svgChart
                    , data = data
                    , country = country
                    }

