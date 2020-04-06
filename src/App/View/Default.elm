module App.View.Default exposing (DefaultParams, view)

import App.Msg exposing (Msg)
import CovidData exposing (..)
import Element exposing (..)
import Element.Region exposing (..)
import Html exposing (Html)
import Html.Events
import Html.Attributes
import Dict
import Array


type alias DefaultParams =
    { leftSide : Html Msg
    , data     : CountriesData
    , country  : String
    }


view : DefaultParams -> Element Msg
view params = 
    el [ width fill, height fill ] <| row [ width fill ] 
        [ column [ mainContent, width <| fillPortion 5, padding 30 ]
            [ paragraph [] [ text <| "Country: " ++ params.country ]
            , paragraph [] 
                [ total_ .confirmed (Dict.get params.country params.data)
                    |> formatNumber
                    |> (++) "Confirmed: "
                    |> text
                ]
            , paragraph [] 
                [ total_ .deaths (Dict.get params.country params.data)
                    |> formatNumber
                    |> (++) "Deaths: "
                    |> text
                ]
            , paragraph []
                [ total_ .recovered (Dict.get params.country params.data)
                    |> formatNumber
                    |> (++) "Recovered: "
                    |> text
                ]
            , el [ width <| maximum 900 fill ] <| html params.leftSide
            ]
        , column [ navigation, width <| fillPortion 2, height fill, padding 30 ]
            [ html <| Html.select [ Html.Events.onInput (\x -> Msg (Ok params.data) x) ]  
                (Dict.keys params.data |> List.map (\x -> Html.option [ Html.Attributes.value x ] [ Html.text x ]))
            , paragraph [] [ text "Total Statisctics" ]
            , paragraph [] 
                [ total .confirmed params.data 
                    |> formatNumber
                    |> (++) "Confirmed: "
                    |> text
                ]
            , paragraph []
                [ total .deaths params.data
                    |> formatNumber
                    |> (++) "Deaths: "
                    |> text
                ]
            , paragraph []
                [ total .recovered params.data
                    |> formatNumber
                    |> (++) "Recovered: "
                    |> text
                ]
            ] 
        ]

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

total : (DataEntry -> Int) -> CountriesData -> Int
total f data = 
    Dict.values data
        |> List.map (total_ f << Just)
        |> List.sum 

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
