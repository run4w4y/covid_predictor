module CovidData.Draw exposing (ChartParams)

import Date exposing (Date)


type alias ChartParams =
    { dataConfirmed : List ( Date, Float )
    , dataRecovered : List ( Date, Float )
    , dataDeaths    : List ( Date, Float )
    , w             : Float
    , h             : Float
    , padding       : Float
    , dateFrom      : Date
    , dateTo        : Date
    , valuesMin     : Int
    , valuesMax     : Int
    }