module App.View.Country exposing (view)

import App.View.Default exposing (..)
import App.Msg exposing (Msg(..))
import CovidData exposing (..)
import CovidData.Draw.Inner exposing (flip)
import Element exposing (..)
import Element.Region exposing (..)
import Element.Background as Bg
import Element.Font as Font
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import Html.Events
import Html.Attributes
import Dict
import Array
import App.View.Fonts exposing (..)

type alias CountryParams = 
    { data     : CountriesData
    , country  : String
    , leftSide : Html Msg
    }


view : CountryParams -> Element Msg
view params = 
    column [ width fill ] 
        [ el [ paddingEach { edges | bottom = 15 } ] <| paragraph 
            [ Font.family
                [ openSans
                , Font.sansSerif
                ]
            , Font.regular
            , Font.size 25
            , Border.solid
            , Border.widthEach { edges | bottom = 2 }
            , Border.color <| rgb255 30 30 30
            , width shrink
            , padding 5
            ] 
            [ el 
                [ Font.color <| rgb255 30 30 30 ] <| text "Country: "
            , el 
                [ Font.color <| rgb255 70 70 70 ] <| text params.country 
            ]
        , paragraph 
            [ Font.family
                [ roboto
                , Font.sansSerif
                ]
            , Font.regular
            , Font.size 18
            ] 
            [ el 
                [ Font.color <| rgb255 183 28 28 ] <| text "Confirmed: "
            , total_ .confirmed (Dict.get params.country params.data)
                |> text 
                |> el [ Font.color <| rgb255 198 40 40 ]
            ]
        , paragraph 
            [ Font.family
                [ roboto
                , Font.sansSerif
                ]
            , Font.regular
            , Font.size 18
            ] 
            [ el
                [ Font.color <| rgb255 13 71 161 ] <| text "Recovered: "
            , total_ .recovered (Dict.get params.country params.data)
                |> text
                |> el [ Font.color <| rgb255 21 101 192 ]
            ]
        , paragraph 
            [ Font.family
                [ roboto
                , Font.sansSerif
                ]
            , Font.regular
            , Font.size 18
            ]  
            [ el
                [ Font.color <| rgb255 38 50 56 ] <| text "Deaths: "
            , total_ .deaths (Dict.get params.country params.data)
                |> text
                |> el [ Font.color <| rgb255 66 66 66 ]
            ]
        , el 
            [ width <| maximum 1000 fill 
            , Font.color <| rgb255 30 30 30
            , Font.family
                [ openSans
                , Font.sansSerif
                ]
            , Font.size 20
            ] <| html params.leftSide
        ]
        |> \x ->
            { data = params.data
            , country = params.country
            , leftSide = x
            }
        |> App.View.Default.view 

edges = 
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }