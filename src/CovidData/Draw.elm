module CovidData.Draw exposing (drawLineChart)

import CovidData exposing (..)
import Axis
import Color
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import Date exposing (Date)
import Time
import Iso8601
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth, width)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))


type alias ChartParams =
    { data      : List ( Date, Float )
    , w         : Float
    , h         : Float
    , padding   : Float
    , dateFrom  : Date
    , dateTo    : Date
    , valuesMin : Int
    , valuesMax : Int
    }


dateToPosix : Date -> Time.Posix
dateToPosix = 
    Date.toIsoString 
        >> Iso8601.toTime 
        >> Result.toMaybe 
        >> Maybe.withDefault (Time.millisToPosix 0) 

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
            Axis.bottom [ Axis.tickCount <| 10 ] xScale

        yAxis : Svg msg
        yAxis =
            Axis.left [ Axis.tickCount <| 10 ] yScale

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
    in
    svg [ viewBox 0 0 params.w params.h ]
        [ g [ transform [ Translate (params.padding - 1) (params.h - params.padding) ] ]
            [ xAxis ]
        , g [ transform [ Translate (params.padding - 1) params.padding ] ]
            [ yAxis ]
        , g [ transform [ Translate params.padding params.padding ], class [ "series" ] ]
            [ Path.element (area params.data) [ strokeWidth 3, fill <| Paint <| Color.rgba 1 0 0 0.54 ]
            , Path.element (line params.data) [ stroke <| Paint <| Color.rgb 1 0 0, strokeWidth 3, fill PaintNone  ]
            ]
        ]
 