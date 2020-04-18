module App.Msg exposing (Msg(..))

import CovidData exposing (CountriesData)
import CovidData.Simulation exposing (SimulationParams)
import Http


type Msg 
    = ShowCountry 
        { data    : Result Http.Error CountriesData
        , country : String
        }
    | ShowSimulation 
        { data    : CountriesData 
        , country : String
        , params  : SimulationParams
        }
    | ShowMap   
        { data    : CountriesData
        , country : String
        }