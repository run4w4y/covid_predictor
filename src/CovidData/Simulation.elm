module CovidData.Simulation exposing (SimulationParams)

type alias SimulationParams = 
    { people : Int
    , initalInfected : Int
    , travelK : Float -- should affect speed or something 
    , radius : Float 
    , infectionP : Float -- probability
    , removeTime : Int -- amount of ticks before the removal
    }