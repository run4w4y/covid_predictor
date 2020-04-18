module App.View.Default exposing (DefaultParams, view, formatNumber, total, total_)

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


type alias DefaultParams =
    { leftSide : Element Msg
    , data     : CountriesData
    , country  : String
    }


view : DefaultParams -> Element Msg
view params = 
    el [ width fill, height fill, scrollbars ] <| row [ width fill, height fill, scrollbars ] 
        -- Main content
        [ column 
            [ mainContent
            , width <| fillPortion 5
            , padding 30
            , Bg.color <| rgb255 250 250 250 
            , height fill
            , scrollbarY
            ]
            [ params.leftSide ]
        
        -- Right bar
        , column 
            [ navigation
            , width <| fillPortion 2
            , height fill
            , padding 30 
            , Bg.color <| rgb255 242 242 242
            ]
            [ row [ width fill ] -- Menu
                [ column [ width <| fillPortion 1, height fill ] 
                    [ Input.button [ width fill, height fill ]
                        { onPress = Just <| ShowCountry { data = Ok params.data, country = params.country }
                        , label = text "Stats"
                        }
                    ]
                , column [ width <| fillPortion 1, height fill ]
                    [ Input.button [ width fill, height fill ]
                        { onPress = Just <| ShowMap { data = params.data, country = params.country }
                        , label = text "Map"
                        }
                    ]
                , column [ width <| fillPortion 1, height fill ]
                    [ Input.button [ width fill, height fill ]
                        { onPress = Nothing
                        , label = text "Simulation"
                        }
                    ]
                ]
            , html <| Html.select [ Html.Events.onInput (\x -> ShowCountry { data = (Ok params.data), country = x }) ]  
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
                , padding 5
                ] 
                [ text "Total Statistics" ]
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

total : (DataEntry -> Int) -> CountriesData -> String
total f data = 
    let
        getRight d l =
            let
                a = Array.fromList l
            in
            Array.get (Array.length a - d) a
                |> Maybe.map f
                |> Maybe.withDefault 0
        t = 
            Dict.values data
                |> List.map (getRight 1)
                |> List.sum 
        p = 
            Dict.values data
                |> List.map (getRight 2)
                |> List.sum
        g = 
            p
                |> toFloat
                |> (flip growth) (toFloat t)
    in
    (formatNumber t) ++ " (+" ++ (formatNumber (t - p)) ++ "/+" ++ (String.fromFloat g) ++ "%)" 

-- Total for countries

total_ : (DataEntry -> Int) -> Maybe (List DataEntry) -> String
total_ f entries =
    let
        e = 
            case entries of 
                Just list ->
                    List.map f list |> Array.fromList
                Nothing -> 
                    [0, 0] |> Array.fromList
        t =
            Array.get (Array.length e - 1) e 
                |> Maybe.withDefault 0
        p =
            Array.get (Array.length e - 2) e 
                |> Maybe.withDefault 0
        g = 
            p
                |> toFloat
                |> (flip growth) (toFloat t)
    in
    (formatNumber t) ++ " (+" ++ (formatNumber (t - p)) ++ "/+" ++ (String.fromFloat g) ++ "%)" 
