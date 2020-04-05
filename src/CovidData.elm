module CovidData exposing (CountriesData, DataEntry)

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
