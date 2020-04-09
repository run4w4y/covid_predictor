module CovidData.Draw.LineChart exposing (drawLineChart)

import CovidData exposing (..)
import Axis
import Color
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import Date exposing (Date)
import Time
import Dict exposing (Dict)
import Array
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox, fontSize)
import TypedSvg.Attributes.InPx exposing (strokeWidth, width)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (Paint(..), Transform(..), Length(..))
import CovidData.Draw exposing (ChartParams)
import CovidData.Draw.Inner exposing (..)


drawLineChart : ChartParams -> Svg msg
drawLineChart params =
    let
        xScale : ContinuousScale Time.Posix
        xScale =
            Scale.time Time.utc ( 0, params.w - 2 * params.padding ) 
                ( dateToPosix params.dateFrom, dateToPosix params.dateTo ) 

        yScale : ContinuousScale Float
        yScale =
            Scale.linear ( params.h - 2 * params.padding, 0 ) 
                ( toFloat params.valuesMin, toFloat params.valuesMax )

        xAxis : Svg msg
        xAxis =
            Axis.bottom [ Axis.tickCount 10 ] xScale

        yAxis : Svg msg
        yAxis =
            Axis.left [ Axis.tickCount 10 ] yScale

        transformToLineData : ( Date, Float ) -> Maybe ( Float, Float )
        transformToLineData ( x, y ) = 
            Just ( Scale.convert xScale (dateToPosix x), Scale.convert yScale y )

        transformToAreaData : ( Date, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
        transformToAreaData ( x, y ) =
            Just
                ( ( Scale.convert xScale (dateToPosix x), Tuple.first (Scale.rangeExtent yScale) )
                , ( Scale.convert xScale (dateToPosix x), Scale.convert yScale y )
                )

        line : List ( Date, Float ) -> Path
        line data =
            List.map transformToLineData data
                |> Shape.line Shape.monotoneInXCurve

        area : List ( Date, Float ) -> Path
        area data =
            List.map transformToAreaData data
                |> Shape.area Shape.monotoneInXCurve

        dataRemoved =
            List.map2 (\(d, x) (_, y) -> (d, x + y)) params.dataRecovered params.dataDeaths
        
        removedDict : Dict String Float
        removedDict =
            List.map (\(x, y) -> (Date.toIsoString x, y)) dataRemoved
                |> Dict.fromList
        
        deathsDict : Dict String Float
        deathsDict =
            List.map (\(x, y) -> (Date.toIsoString x, y)) params.dataDeaths
                |> Dict.fromList

        areaConfirmed : Path
        areaConfirmed =
            (\(x, y) -> Just 
                ( 
                    ( Scale.convert xScale (dateToPosix x)
                    , Date.toIsoString x 
                        |> (flip Dict.get) removedDict 
                        |> Maybe.withDefault 0 
                        |> Scale.convert yScale 
                    )
                , 
                    ( Scale.convert xScale (dateToPosix x)
                    , Scale.convert yScale y 
                    )
                )
            )
                |> (flip List.map) params.dataConfirmed
                |> Shape.area Shape.monotoneInXCurve
        
        areaRecovered : Path
        areaRecovered =
            (\(x, y) -> Just 
                ( 
                    ( Scale.convert xScale (dateToPosix x)
                    , Date.toIsoString x 
                        |> (flip Dict.get) deathsDict 
                        |> Maybe.withDefault 0 
                        |> Scale.convert yScale 
                    )
                , 
                    ( Scale.convert xScale (dateToPosix x)
                    , Scale.convert yScale y 
                    )
                )
            )
                |> (flip List.map) dataRemoved
                |> Shape.area Shape.monotoneInXCurve

        getLast : List a -> Maybe a
        getLast l =
            let
                arr = Array.fromList l
            in
            Array.get (Array.length arr - 1) arr 

        deathsY : Float
        deathsY = 
            List.unzip params.dataDeaths
                |> Tuple.second
                |> getLast
                |> Maybe.withDefault 0
                |> Scale.convert yScale
            
        removedY : Float
        removedY =
            List.unzip dataRemoved
                |> Tuple.second
                |> getLast
                |> Maybe.withDefault 0
                |> Scale.convert yScale
            
        confirmedY : Float
        confirmedY =
            List.unzip params.dataConfirmed
                |> Tuple.second
                |> getLast
                |> Maybe.withDefault 0
                |> Scale.convert yScale
    in
    svg [ viewBox 0 0 params.w params.h ]
        [ g [ transform [ Translate (params.padding - 1) (params.h - params.padding) ] ] 
            [ xAxis 
            , text_ -- Text "Time"
                [ transform [ Translate ( params.w / 2 - params.padding ) 40 ]
                , fontSize <| Px 12
                ] 
                [ text "Time" ]
            ]
        , g [ transform [ Translate (params.padding - 1) params.padding ] ]
            [ yAxis 
            , text_ -- Text "People"
                [ transform [ Translate -30 -15 ]
                , fontSize <| Px 12
                ] 
                [ text "People" ]
            , text_
                [ transform [ Translate (params.w - 2*params.padding + 2) (deathsY + 4) ] 
                , fontSize <| Px 12
                ]
                [ text "- D" ]
            , text_
                [ transform [ Translate (params.w - 2*params.padding + 2) (removedY + 4) ] 
                , fontSize <| Px 12
                ]
                [ text "- R" ]
            , text_
                [ transform [ Translate (params.w - 2*params.padding + 2) (confirmedY + 4) ] 
                , fontSize <| Px 12
                ]
                [ text "- C" ]
            , text_
                [ transform [ Translate 10 10 ] 
                , fontSize <| Px 12
                ]
                [ text "C - Confirmed" ]
            , text_
                [ transform [ Translate 10 25 ] 
                , fontSize <| Px 12
                ]
                [ text "R - Removed (D + recovered)" ]
            , text_
                [ transform [ Translate 10 40 ] 
                , fontSize <| Px 12
                ]
                [ text "D - Deaths" ]
            ]
        -- Areas
        , g [ transform [ Translate params.padding params.padding ], class [ "series" ] ]
            [ Path.element (area params.dataDeaths)
                [ strokeWidth 3, fill <| Paint <| Color.rgba 0 0 0 0.54 ]
            , Path.element areaConfirmed 
                [ strokeWidth 3, fill <| Paint <| Color.rgba 1 0 0 0.54 ]
            , Path.element areaRecovered
                [ strokeWidth 3, fill <| Paint <| Color.rgba 0 0 1 0.54 ]
            ]
        -- Lines
        , g [ transform [ Translate params.padding params.padding ], class [ "series" ] ]
            [ Path.element (line params.dataDeaths)
                [ stroke <| Paint <| Color.rgb 0 0 0, strokeWidth 3, fill PaintNone ]
            , Path.element (line dataRemoved)
                [ stroke <| Paint <| Color.rgb 0 0 1, strokeWidth 3, fill PaintNone ]
            , Path.element (line params.dataConfirmed) 
                [ stroke <| Paint <| Color.rgb 1 0 0, strokeWidth 3, fill PaintNone ]
            ]
        ]
