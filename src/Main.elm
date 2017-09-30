module Main exposing (..)

import Map exposing (..)
import Html
import Geo exposing (espg3857, LatLng, Point)
import VectorLayer exposing (..)
import Pane exposing (PaneAction(..))
import Json.Decode as D
import GeoJson as GJ
import Dict
import Http
import Result
import String
import Array
import Util
import Task

-- MODEL

getPoint : LatLng ->  String
getPoint ll = String.join "," <| List.map toString [ll.lat,ll.lng]

convert : Point -> GJ.Position
convert {x,y} = (x,y,0)

whitneyBox = GJ.LineString  <| List.map convert [ 
          { y = 36.577, x = -118.302 }
        , { y = 36.579, x = -118.302 }
        , { y = 36.579, x = -118.300 }
        , { y = 36.577, x = -118.300 }
        , { y = 36.577, x = -118.302 }
        ]



{-
getGeometryZ : Bounds LatLng -> Cmd (List GJ.Geometry)
getGeometryZ bounds = 
  let getGeom = D.map (Dict.values >> (List.map .geometry))
  in Http.send (Result.withDefault []) <| Http.get ("http://localhost:8081/segment/bounds/" ++ getPoint bounds.sw ++ "/" ++ getPoint bounds.ne ) <| getGeom Util.segmentDictDecoder
-}


getGeometry : Bounds LatLng -> Cmd (List GJ.Geometry)
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
