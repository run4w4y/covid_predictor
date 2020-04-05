module CovidData exposing (DataEntry, getData, totalData, getCountryData)

import Dict exposing (Dict, values)
import Maybe
import Result
import Http
import Date
import Time exposing (Month)
import Json.Decode as D
import Json.Encode as E


-- Types

type alias Country = 
    String

type alias DataEntry =
    { day       : Maybe Date
    , confirmed : Int
    , deaths    : Int
    , recovered : Int
    }

type alias CountriesData = 
    Dict Country DataEntry


-- JSON Decoders

dateDecoder : D.Decoder Date
dateDecoder =
    D.map 
        (Maybe.withDefault (Date.fromCalendarDate 2020 Jan 1) 
            << Result.toMaybe 
            << Date.fromIsoString) 
        string

entryDecoder : D.Decoder DataEntry
entryDecoder =
    D.map4 DataEntry
        (D.field "date" dateDecoder)
        (D.field "confirmed" D.int)
        (D.field "deaths" D.int)
        (D.field "recovered" D.int)

dataDecoder : D.Decoder CountriesData
dataDecoder = 
    D.dict entryDecoder


-- JSON Encode functions

dateEncode : Date -> E.Value
dateEncode = -- point free function, not sure if works
    E.string <| Date.toIsoString

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


-- HTTP request for JSON data

getData : Cmd (Result Http.Error CountriesData)
getData = 
    Http.get 
        { url = "https://pomber.github.io/covid19/timeseries.json"
        , expect = Http.expectJson identity dataDecoder
        }
