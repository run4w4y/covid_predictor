module CovidData.Draw.BarChart exposing (drawBarChart)

import CovidData exposing (..)
import CovidData.Draw exposing (ChartParams)
import CovidData.Draw.Inner exposing (..)
import Date exposing (Date)
import Axis
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Time exposing (Month(..))
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..))


drawBarChart : ChartParams -> Svg msg
drawBarChart params = 
    let 
        growthRates_ : List ( Date, Float ) -> List ( Date, Float )
        growthRates_ l =
            case l of 
                ( _, x ) :: ( d, y ) :: xs -> 
                    (d, growth x y) :: (growthRates_ xs)
                _ ->
                    []

        growthRates : List ( Date, Float )
        growthRates = 
            growthRates_ params.dataConfirmed 

        xScale : BandScale Date
        xScale =
            List.map Tuple.first growthRates
                |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } 
                    ( 0, params.w - 2 * params.padding )
        
        yScale : ContinuousScale Float
        yScale =
            Scale.linear ( params.h - 2 * params.padding, 0 ) 
                ( 0, Maybe.withDefault 0 <| List.maximum <| List.map Tuple.second growthRates )
        
        dateFormat : Date -> String
        dateFormat =
            Date.format ""
        
        xAxis : Svg msg
        xAxis = 
            Axis.bottom [ Axis.tickCount 10 ] <| Scale.toRenderable dateFormat xScale
        
        yAxis : Svg msg
        yAxis =
            Axis.left [ Axis.tickCount 10 ] yScale

        column : BandScale Date -> ( Date, Float ) -> Svg msg
        column scale ( date, value ) = 
            g [ class [ "column" ] ]
                [ rect
                    [ x <| Scale.convert xScale date
                    , y <| Scale.convert yScale value
                    , width <| Scale.bandwidth xScale
                    , height <| params.h - Scale.convert yScale value - 2 * params.padding
                    ]
                    []
                , text_
                    [ x <| Scale.convert (Scale.toRenderable dateFormat scale) date
                    , y <| Scale.convert yScale value - 5
                    , textAnchor AnchorMiddle
                    ]
                    [ text <| String.fromFloat value ]
                ]
    in
    svg [ viewBox 0 0 params.w params.h ]
        [ style [] [ text """
            .column rect { fill: rgba(118, 214, 78, 0.8); }
            .column text { display: none; }
            .column:hover rect { fill: rgb(118, 214, 78); }
            .column:hover text { display: inline; }
          """ ]
        , g [ transform [ Translate (params.padding - 1) (params.h - params.padding) ] ]
            [ xAxis ]
        , g [ transform [ Translate (params.padding - 1) params.padding ] ]
            [ yAxis ]
        , g [ transform [ Translate params.padding params.padding ], class [ "series" ] ] <|
            List.map (column xScale) growthRates
        ]
