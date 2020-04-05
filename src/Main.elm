module Main exposing (main)

import Browser
import Html exposing (Html, text, pre)
import CovidData exposing (..)
import Json.Encode as E
import Http


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
    = Failure
    | Loading
    | Success String

init : () -> (Model, Cmd Msg)
init _ = 
    ( Loading
    , loadData
    )

loadData : Cmd Msg
loadData =
    Http.get 
        { url = "https://pomber.github.io/covid19/timeseries.json"
        , expect = Http.expectJson GotData dataDecoder
        }


-- Update

type Msg
    = GotData (Result Http.Error CountriesData)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotData result -> 
            case result of 
                Ok data -> 
                    (Success <| E.encode 4 <| dataEncode data, Cmd.none)
                Err _ ->
                    (Failure, Cmd.none)


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- View

view : Model -> Html Msg
view model =
    case model of
        Failure ->
            pre [] [ text "something went wrong" ]
        Loading ->
            pre [] [ text "loading" ]
        Success result ->
            pre [] [ text result ]
