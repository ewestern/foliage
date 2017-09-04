module Main exposing (..)

import Map exposing (..)
import Html
import Geo exposing (espg3857, LatLng)
import VectorLayer exposing (..)
import Pane exposing (PaneAction(..))
import Types exposing (..)
import Json.Decode as D
import Dict
import Http
import Result
import GeoJSON
import String
import Array
import Util
import Task

-- MODEL

getPoint : LatLng ->  String
getPoint ll = String.join "," <| List.map toString [ll.lat,ll.lng]

whitneyBox = Geometry_LineString (LineString (Array.fromList [ 
          { y = 36.577, x = -118.302 }
        , { y = 36.579, x = -118.302 }
        , { y = 36.579, x = -118.300 }
        , { y = 36.577, x = -118.300 }
        , { y = 36.577, x = -118.302 }
        ]))



getGeometryZ : Bounds LatLng -> Cmd (List Geometry)
getGeometryZ bounds = 
  let getGeom = D.map (Dict.values >> (List.map .geometry))
  in Http.send (Result.withDefault []) <| Http.get ("http://localhost:8081/segment/bounds/" ++ getPoint bounds.sw ++ "/" ++ getPoint bounds.ne ) <| getGeom Util.segmentDictDecoder

testGeom = Geometry_LineString (LineString (Array.fromList [{ x = -118.02928544052078, y = 36.41785245831469 },{ x = -118.02920245967934, y = 36.41786369006496 },{ x = -118.02898830205326, y = 36.41789269144992 },{ x = -118.0288879706723, y = 36.41790627013306 },{ x = -118.02878244251131, y = 36.41792043554942 },{ x = -118.02860524907824, y = 36.417944491611536 }]))

testGeom2 = Geometry_LineString (LineString (Array.fromList [ 
          { y = 36.67584656386455, x = -118.37332275390625 }
        , { y = 37.22458044629443, x = -118.37332275390625 }
        , { y = 37.22458044629443, x = -117.68667724609377 }
        , { y = 36.67584656386455, x = -117.68667724609377 }
        , { y = 36.67584656386455, x = -118.37332275390625 }
        ]) )

getGeometry : Bounds LatLng -> Cmd (List Geometry)
getGeometry _ = 
        let gt = Task.succeed [whitneyBox]
        in Task.perform identity gt



---- UPDATE
mkDefaultOptions crs size ll zoom = 
  { size = size
  , initialCoords = ll
  , crs = crs
  , vectorOptions = {stroke = "#3388ff", color="", weight="" , getGeometry=getGeometry}
  , initialZoom = zoom
  , tileUrl = Just "https://api.mapbox.com/styles/v1/mapbox/outdoors-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiZXdlc3Rlcm4iLCJhIjoiY2lmY2Z5eWNsM3Y2OHN4bTdndmJha29kZCJ9.8hIQ8iTAmMZD__3uHytwvw" }


  -- , vectorLayers = [mkVectorLayer crs size ll zoom]
init : (Map, Cmd Action)
init = 
-- 36.5784983,-118.3010362
  let mOpts = mkDefaultOptions  espg3857 {x = 500, y = 300 } (Initial_Center { lat = 36.5784983, lng=-118.3010362 }) 12
      --vOpts = {stroke = "#3388ff", color="", weight="" , getGeometry=getGeometryZ}
      f = A << Pane_Vector << VectorLayer_Geometry 
      ib = getInitialBounds mOpts.size mOpts.crs mOpts.initialZoom  mOpts.initialCoords
  in (makeMap mOpts, Cmd.map f <| getGeometry ib  )


main =
  Html.program
    { init = init
    , view = mapView
    , update = updateMap
    , subscriptions = mapSubscriptions
    }
