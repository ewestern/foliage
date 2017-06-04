module Main exposing (..)

import Map exposing (..)
import Html

-- MODEL

---- UPDATE
defaultOptions = 
  { size = {x = 500, y = 300 }
  , initialCoords = Initial_Bounds { sw =  { lat = 36.1, lng=-118.5 }, ne = { lat = 36.2, lng= -118.2 } }
  , initialZoom = 10
  --, crs = espg4326
  , tileUrl = Just "https://api.mapbox.com/styles/v1/mapbox/outdoors-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiZXdlc3Rlcm4iLCJhIjoiY2lmY2Z5eWNsM3Y2OHN4bTdndmJha29kZCJ9.8hIQ8iTAmMZD__3uHytwvw" }


init : (Map, Cmd Action)
init = (makeMap defaultOptions, Cmd.none)

main =
  Html.program
    { init = init
    , view = mapView
    , update = updateMap
    , subscriptions = mapSubscriptions
    }
