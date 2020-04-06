module App.Model exposing (Model(..))

import CovidData exposing (CountriesData)
import Html exposing (Html)
import App.Msg exposing (Msg)


type Model 
    = HttpFailure
    | CountryNotFound CountriesData String
    | Loading
    | Success CountriesData String (Html Msg)