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
import Layer exposing (pointToLatLng, latLngToPoint, getBounds, getOriginFromCenter)


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
getInitialBounds : Size -> CRS -> Zoom -> InitialCoords -> Bounds LatLng
getInitialBounds size crs zoom ic = 
    case ic of
        Initial_Center ll -> 
                let or = getOriginFromCenter crs zoom size ll
                in getBounds crs zoom size or

        Initial_Bounds bounds -> bounds


makeMap : MapOptions -> Map
makeMap mo = 
  let pane = makePane mo
  in { size = mo.size, pane = pane }

mkVectorLayer vectorOptions crs size ll zoom = 
  {
    options = vectorOptions
  , geometry = []
  , size = size
  , latLngOrigin = ll
  , crs = crs
  , currentZoom = zoom
  }

makePane : MapOptions -> MapPane
makePane mo = 
    let {sw,ne} = getInitialBounds mo.size mo.crs mo.initialZoom mo.initialCoords
        initialLayer = 
          Maybe.map (\url -> 
              { urlTemplate = url, size = mo.size, levels = Dict.empty, crs=mo.crs, currentZoom = mo.initialZoom, latLngOrigin=sw }) mo.tileUrl
        -- convert vectorLayerOptions into vectoryLayers
        --vls = 
    in
      { dragstate = Nothing
      , size = mo.size
      , position = emptyPos -- the origin of the pane, relative to the origin of the map, in pixels
      , latLngCenter = sw -- ??????????
      , tileLayers = List.map (updateTileLayer (TileLayer_Move {x=0, y=0})) <| catMaybe [initialLayer] 
      , vectorLayers = [mkVectorLayer mo.vectorOptions mo.crs mo.size sw mo.initialZoom] }

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
