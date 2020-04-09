module CovidData.Draw.Inner exposing (dateToPosix, flip)

import Date exposing (Date)
import Time
import Iso8601


dateToPosix : Date -> Time.Posix
dateToPosix = 
    Date.toIsoString 
        >> Iso8601.toTime 
        >> Result.toMaybe 
        >> Maybe.withDefault (Time.millisToPosix 0) 

flip : (a -> b -> c) -> (b -> a -> c)
flip f =
    \x y -> f y x