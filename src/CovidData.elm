module CovidData exposing (DataEntry, getData, totalData, getCountryData)

import Dict
import Http
import Date
import Json.Decode as D
import Json.Encode as E


-- Types

type alias Country = 
    String

type alias DataEntry =
    { day       : Date
    , confirmed : Int
    , deaths    : Int
    , recovered : Int
    }

type alias CountriesData = 
    Dict Country DataEntry


-- JSON Decoders

dateDecoder : D.Decoder (Result String Date) -- do something about it 
dateDecoder =
    D.map Date.fromIsoString string

entryDecoder : D.Decoder DataEntry
entryDecoder =
    D.map4 DataEntry
        (D.field "date" dateDecoder)
        (D.field "confirmed" D.int)
        (D.field "deaths" D.int)
        (D.field "recovered" D.int)

-- TODO: turn type into D.Decoder (Maybe CountriesData) cause that would make more sense
dataDecoder : D.Decoder CountriesData 
dataDecoder = 
    D.dict entryDecoder


-- JSON Encode functions

dateEncode : Date -> E.Value
dateEncode = -- point free function, not sure if works
    E.string <| Date.format "y-M-d"

entryEncode : DataEntry -> E.Value
entryEncode entry =
    E.object 
        [ ("date", dateEncode entry.day)
        , ("confirmed", E.int entry.confirmed)
        , ("deaths", E.int entry.deaths)
        , ("recovered", E.int entry.recovered)
        ]

dataEncode : CountriesData -> E.Value
dataEncode = -- point free again
    E.dict identity entryEncode

getData : Cmd (Result Http.Error CountriesData)

