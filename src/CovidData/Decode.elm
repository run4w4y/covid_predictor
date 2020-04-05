module CovidData.Decode exposing (dataDecoder)

import CovidData exposing (CountriesData, DataEntry)
import Date exposing (Date)
import Time exposing (Month(..))
import Json.Decode as D
import List
import Array
import String
import Maybe


-- JSON Decoders

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