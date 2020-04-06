module App.View.Error exposing (view)

import App.Msg exposing (Msg)
import Element exposing (..)


view : String -> Element Msg
view errorMessage = 
    el [] <| paragraph []
        [ text errorMessage ]