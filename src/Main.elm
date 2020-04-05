module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import CovidData exposing (..)
import Json.Encode as E
import Http
import Dict
import List
import Platform.Cmd as C


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
    | Success CountriesData String

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
                Just entry ->
                    ( entry
                        |> E.list entryEncode
                        |> E.encode 4
                        |> Success data 
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
            defaultLayout data "country could not be found"
        Success data result ->
            defaultLayout data result

defaultLayout : CountriesData -> String -> Html Msg
defaultLayout data t = 
    div [] 
        [ select [ onInput (\x -> Msg (Ok data) x) ]  
            (Dict.keys data |> List.map (\x -> option [ value x ] [ text x ]))
        , pre [] [ text t ]
        ]