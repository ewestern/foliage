module Main exposing (..)

import Map exposing (..)
import Html

-- MODEL

--type alias Map = { }


---- UPDATE



init : (Map, Cmd Action)
init = (defaultMap, Cmd.none)

main =
  Html.program
    { init = init
    , view = mapView
    , update = updateMap
    , subscriptions = mapSubscriptions
    }
