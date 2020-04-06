module App.View.Fonts exposing (roboto, openSans)

import Element.Font exposing (..)


roboto : Font
roboto = 
    external 
        { name = "Roboto"
        , url = "https://fonts.googleapis.com/css?family=Roboto"
        }

openSans : Font
openSans = 
    external
        { name = "Open Sans"
        , url = "https://fonts.googleapis.com/css?family=Open+Sans"
        }