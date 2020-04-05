module CovidData.Encode exposing (dataEncode, entryEncode)

import CovidData exposing (CountriesData, DataEntry)
import Date exposing (Date)
import Json.Encode as E


-- JSON Encode functions

dateEncode : Date -> E.Value
dateEncode = 
    E.string << Date.toIsoString

entryEncode : DataEntry -> E.Value
entryEncode entry =
    E.object 
        [ ("date", dateEncode entry.date)
        , ("confirmed", E.int entry.confirmed)
        , ("deaths", E.int entry.deaths)
        , ("recovered", E.int entry.recovered)
        ]

dataEncode : CountriesData -> E.Value
dataEncode = 
    E.dict identity <| E.list entryEncode
