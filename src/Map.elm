module Map exposing (..)

import Mouse exposing (Position)
import Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Dict


-- 

import Pane exposing (..)
import Geo exposing (..)
--LatLng, Bounds, CRS, pointToLatLng, latLngToPoint, mapCoord
import Util exposing (..)
import TileLayer exposing (TileLayer, moveLayer)


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
  , initialZoom : Zoom
  --, crs : CRS
  --, initialZoom : Zoom
  --, vectorLayers = List VectorLayer 
  , size : Size }
  

getInitialOrigin : Size -> CRS -> Zoom -> InitialCoords -> LatLng
getInitialOrigin sz crs zoom ic = case ic of 
    Initial_Center ll -> 
      let p = latLngToPoint crs zoom ll
          p2 = difference p <| mapCoord ((*) 0.5 << toFloat) sz
      in pointToLatLng crs zoom p2
    Initial_Bounds bs -> bs.sw


makeMap : MapOptions -> Map
makeMap mo = 
  let pane = makePane mo
  in 
    { size = mo.size
    , pane = pane }

--getInitialOrigin : Size -> CRS -> Zoom -> InitialCoords -> (Point, LatLng)
--makePane : Size -> Maybe String -> InitialCoords -> Zoom -> MapPane
--makePane s tu ic zoom = 
makePane : MapOptions-> MapPane
makePane mo = 
    let crs = espg3857
        ll = getInitialOrigin mo.size crs mo.initialZoom mo.initialCoords
        initialLayer = 
          Maybe.map (\url -> 
              { urlTemplate = url, size = mo.size, levels = Dict.empty, crs=crs, currentZoom = mo.initialZoom, latLngOrigin=ll }) mo.tileUrl
    in
      { dragstate = Nothing
      , size = mo.size
      , position = emptyPos -- the origin of the pane, relative to the origin of the map, in pixels
      , latLngCenter = ll
      , tileLayers = List.map (moveLayer {x=0, y=0}) <| catMaybe [initialLayer] 
      , vectorLayers = [] }

-- paneView : MapPane -> Html PaneAction
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


--updatePane : DragAction -> MapPane -> (MapPane, Cmd DragAction)
updateMap : Action -> Map -> (Map, Cmd Action)
updateMap action map = 
  case action of
    A da -> 
      let (mp, cd) = updatePane da map.pane
          nm = { map | pane = mp }
          cmd  = Cmd.map A cd
      in (nm, cmd)

--dragSubscription : DragState -> Sub DragAction

mapSubscriptions  : Map -> Sub Action
mapSubscriptions map = 
     Sub.map A <| dragSubscription map.pane.dragstate 
