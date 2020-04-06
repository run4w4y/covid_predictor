module App.View.Loading exposing (view)

import App.Msg exposing (Msg)
import Element exposing (..)


view : Element Msg
view = 
    el [] <| paragraph []
        [ text "Loading..." ]
