module App.Msg exposing (Msg)

import CovidData exposing (CountriesData)
import Http


type alias Msg = 
    { data    : Result Http.Error CountriesData
    , country : String
    }