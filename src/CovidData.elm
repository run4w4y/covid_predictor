module CovidData exposing (CountriesData, DataEntry, dataDecoder, dataEncode, entryEncode)

import Dict exposing (Dict, values)
import Maybe
import Result
import Date exposing (Date)
import Time exposing (Month(..))
import Json.Decode as D
import Json.Encode as E
import List
import Array
import String


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


-- JSON Decoders

stringToDate : String -> Date
stringToDate s = 
    let
        t = 
            String.split "-" s 
                |> List.map (Maybe.withDefault 0 << String.toInt)
                |> Array.fromList
        year = 
            Array.get 0 t 
                |> Maybe.withDefault 2020
        month = 
            Array.get 1 t 
                |> Maybe.withDefault 1
                |> Date.numberToMonth 
        day = 
            Array.get 2 t 
                |> Maybe.withDefault 1
    in
    Date.fromCalendarDate year month day

dateDecoder : D.Decoder Date
dateDecoder =
    D.map 
        stringToDate
        D.string

entryDecoder : D.Decoder DataEntry
entryDecoder =
    D.map4 DataEntry
        (D.field "date" dateDecoder)
        (D.field "confirmed" D.int)
        (D.field "deaths" D.int)
        (D.field "recovered" D.int)

dataDecoder : D.Decoder CountriesData
dataDecoder = 
    D.dict <| D.list entryDecoder


-- JSON Encode functions

dateEncode : Date -> E.Value
dateEncode = -- point free function, not sure if works
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
dataEncode = -- point free again
    E.dict identity <| E.list entryEncode
