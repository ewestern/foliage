module Map exposing (..)

import Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Dict
---
import Pane exposing (..)
import Geo exposing (..)
import Util exposing (..)
import TileLayer exposing (TileLayer, updateTileLayer, TileLayerAction(..))
import VectorLayer exposing (VectorLayer, VectorOptions)


type alias Map
  = { size : Size
    ,  pane : MapPane }

type InitialCoords
  = Initial_Center LatLng
  | Initial_Bounds (Bounds LatLng)

type Action
    = A PaneAction


type alias MapOptions =
  { tileUrl : Maybe String
  , initialCoords : InitialCoords
  , vectorOptions : VectorOptions
  , initialZoom : Zoom
  , crs : CRS
  , size : Size }
  

-- TODO
getInitialOrigin : Size -> CRS -> Zoom -> InitialCoords -> LatLng
getInitialOrigin sz crs zoom ic = case ic of 
    Initial_Center ll -> 
      let p = crs.projection.project ll
          p2 = difference p <| mapCoord ((*) 0.5 << toFloat) sz
      in crs.projection.unproject p2
    Initial_Bounds bs -> bs.sw


makeMap : VectorOptions -> MapOptions -> Map
makeMap vo mo = 
  let pane = makePane vo mo
  in { size = mo.size, pane = pane }

mkVectorLayer vectorOptions crs size ll zoom = 
  {
    options = vectorOptions
  , geometry = []
  , size = size
  , lastFetch = {x=0,y=0}
  , latLngOrigin = ll
  , crs = crs
  , currentZoom = zoom
  }

makePane : VectorOptions -> MapOptions -> MapPane
makePane vo mo = 
    let ll = getInitialOrigin mo.size mo.crs mo.initialZoom mo.initialCoords
        initialLayer = 
          Maybe.map (\url -> 
              { urlTemplate = url, size = mo.size, levels = Dict.empty, crs=mo.crs, currentZoom = mo.initialZoom, latLngOrigin=ll }) mo.tileUrl
        -- convert vectorLayerOptions into vectoryLayers
        --vls = 
    in
      { dragstate = Nothing
      , size = mo.size
      , position = emptyPos -- the origin of the pane, relative to the origin of the map, in pixels
      , latLngCenter = ll
      , tileLayers = List.map (updateTileLayer (TileLayer_Move {x=0, y=0})) <| catMaybe [initialLayer] 
      , vectorLayers = [mkVectorLayer vo mo.crs mo.size ll mo.initialZoom] }

mapView : Map -> Html.Html Action
mapView  map =  
      div
        [ style
            [ ("height", px map.size.y)
            , ("width", px map.size.x)
            , ("position", "relative")
            , ("overflow", "hidden")
            ]
        ] 
        [ Html.map A <| viewPane map.pane ]


updateMap : Action -> Map -> (Map, Cmd Action)
updateMap action map = 
  case action of
    A da -> 
      let (mp, cd) = updatePane da map.pane
          nm = { map | pane = mp }
          cmd  = Cmd.map A cd
      in (nm, cmd)

mapSubscriptions  : Map -> Sub Action
mapSubscriptions map = 
     Sub.map (A << Pane_Drag) <| dragSubscription map.pane.dragstate 
