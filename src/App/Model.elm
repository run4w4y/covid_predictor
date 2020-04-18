module App.Model exposing (Model(..))

import CovidData exposing (CountriesData)
import CovidData.Simulation exposing (SimulationParams)
import Html exposing (Html)
import App.Msg exposing (Msg)


type Model 
    = HttpFailure
    | CountryNotFound CountriesData String
    | Loading
    | DisplayCountry CountriesData String (Html Msg)
    | Simulation CountriesData String SimulationParams
    | Map CountriesData String