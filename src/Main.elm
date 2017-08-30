module Main exposing (..)

import Map exposing (..)
import Html
import Geo exposing (espg3857)
import VectorLayer exposing (..)
import Types exposing (..)
import Json.Decode as D
import Dict
import Http
import Result
import GeoJSON
import String
import Util

-- MODEL

getPoint : LatLng ->  String
getPoint ll = String.join "," <| List.map toString [ll.lat,ll.lng]



getGeometry : Bounds LatLng -> Cmd (List Geometry)
getGeometry bounds = 
  let getGeom = D.map (Dict.values >> (List.map .geometry))
  in Http.send (Result.withDefault []) <| Http.get ("http://localhost:8081/segment/bounds/" ++ getPoint bounds.sw ++ "/" ++ getPoint bounds.ne ) <| getGeom Util.segmentDictDecoder
-- type alias VectorLayer -- 
--   = { options: VectorOptions
--     , geometry :  List Geometry
--     , size : Size -- needed (in theory) to calculate when a layer comes  in view
--     , latLngOrigin : LatLng
--     , crs : CRS
--     , currentZoom : Zoom }






---- UPDATE
mkDefaultOptions crs size ll zoom = 
  { size = size
  , initialCoords = ll
  , crs = crs
  , vectorOptions = {stroke = "", color="", weight="" , getGeometry=getGeometry, fetchThreshold=100}
  , initialZoom = zoom
  , tileUrl = Just "https://api.mapbox.com/styles/v1/mapbox/outdoors-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiZXdlc3Rlcm4iLCJhIjoiY2lmY2Z5eWNsM3Y2OHN4bTdndmJha29kZCJ9.8hIQ8iTAmMZD__3uHytwvw" }


  -- , vectorLayers = [mkVectorLayer crs size ll zoom]
init : (Map, Cmd Action)
init = 
  let mOpts = mkDefaultOptions  espg3857 {x = 500, y = 300 } (Initial_Center { lat = 36.1, lng=-118.5 }) 10 
      vOpts = {stroke = "", color="", weight="" , getGeometry=getGeometry, fetchThreshold=10.0}
  in (makeMap vOpts mOpts, Cmd.none)


main =
  Html.program
    { init = init
    , view = mapView
    , update = updateMap
    , subscriptions = mapSubscriptions
    }
