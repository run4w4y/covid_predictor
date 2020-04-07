module App.View.Default exposing (DefaultParams, view)

import App.Msg exposing (Msg)
import CovidData exposing (..)
import Element exposing (..)
import Element.Region exposing (..)
import Element.Background as Bg
import Element.Font as Font
import Element.Border as Border
import Html exposing (Html)
import Html.Events
import Html.Attributes
import Dict
import Array
import App.View.Fonts exposing (..)


type alias DefaultParams =
    { leftSide : Html Msg
    , data     : CountriesData
    , country  : String
    }


view : DefaultParams -> Element Msg
view params = 
    el [ width fill, height fill ] <| row [ width fill, height fill ] 
        -- Main content
        [ column 
            [ mainContent
            , width <| fillPortion 5
            , padding 30
            , Bg.color <| rgb255 224 224 224 
            , height fill
            ]
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
                    |> formatNumber
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
                    |> formatNumber
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
                    |> formatNumber
                    |> text
                    |> el [ Font.color <| rgb255 66 66 66 ]
                ]
            , el [ width <| maximum 900 fill ] <| html params.leftSide
            ]
        
        -- Right bar
        , column 
            [ navigation
            , width <| fillPortion 2
            , height fill
            , padding 30 
            , Bg.color <| rgb255 242 242 242
            ]
            [ html <| Html.select [ Html.Events.onInput (\x -> Msg (Ok params.data) x) ]  
                (Dict.keys params.data 
                    |> List.map (\x -> Html.option [ Html.Attributes.value x ] [ Html.text x ]))
            , el [ paddingEach { edges | bottom = 15, top = 15 } ] <| paragraph 
                [ Font.color <| rgb255 30 30 30
                , Font.regular
                , Font.family
                    [ openSans
                    , Font.sansSerif
                    ]
                , Font.size 25
                , Border.solid
                , Border.widthEach { edges | bottom = 2 }
                , Border.color <| rgb255 30 30 30
                ] 
                [ text "Total Statisctics" ]
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
                , total .confirmed params.data
                    |> formatNumber
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
                , total .recovered params.data
                    |> formatNumber
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
                , total .deaths params.data
                    |> formatNumber
                    |> text
                    |> el [ Font.color <| rgb255 66 66 66 ]
                ]
            ]
        ]

edges = 
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }

formatNumber : Int -> String
formatNumber n =
    let 
        breakString : List String -> String -> List String
        breakString acc s =
            case s of 
                "" -> 
                    acc
                _  ->
                    breakString ((String.right 3 s)::acc) <| String.dropRight 3 s
    in
    String.fromInt n
        |> breakString []
        |> String.join " "

-- Total like total

total : (DataEntry -> Int) -> CountriesData -> Int
total f data = 
    Dict.values data
        |> List.map (total_ f << Just)
        |> List.sum 

-- Total for countries

total_ : (DataEntry -> Int) -> Maybe (List DataEntry) -> Int
total_ f entries =
    case entries of
        Just e -> 
            let 
                e_ = 
                    List.map f e
                        |> Array.fromList
            in
            Array.get (Array.length e_ - 1) e_ |>
                Maybe.withDefault 0
        Nothing -> 
            0
