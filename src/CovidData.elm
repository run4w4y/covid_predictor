module CovidData exposing (CountriesData, DataEntry, growth)

import Dict exposing (Dict)
import Date exposing (Date)


-- Types

type alias Country = 
    String

type alias DataEntry =
    { date      : Date
    , confirmed : Int
    , deaths    : Int
    , recovered : Int
    }

type alias CountriesData = 
    Dict Country (List DataEntry)


growth : Float -> Float -> Float
growth a b =
    if (a == 0) 
        then 0
        else ((b / a) - 1) * 100 |> round |> toFloat