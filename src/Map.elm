module Map exposing (..)

import Mouse exposing (Position)
import Html

import Html exposing (..)
import Html.Attributes exposing (..)
import Dict


-- 

import Pane exposing (..)
import Geo exposing (LatLng, Bounds)
import Util exposing (..)
import TileLayer exposing (TileLayer)


type alias Map
  = { size : Size
    ,  pane : MapPane }

type InitialCoords
  = Initial_Center LatLng
  | Initial_Bounds (Bounds LatLng)

type Action
    = A DragAction


type alias MapOptions =
  { tileUrl : Maybe String
  , initialCoords : InitialCoords
  , initialZoom : Zoom
  --, vectorLayers = List VectorLayer 
  , size : Size }
  

getInitialCenter : InitialCoords -> LatLng
getInitialCenter ic = case ic of
    Initial_Center ll -> ll
    Initial_Bounds bs ->  
      let nlat = (bs.ne.lat + bs.sw.lat) / 2
          nlng = (bs.ne.lng + bs.sw.lng) / 2
      in {lat  = nlat, lng = nlng }

makeMap : MapOptions -> Map
makeMap mo = 
  let pane = makePane mo.size mo.tileUrl mo.initialCoords mo.initialZoom
  in 
    { size = mo.size
    , pane = pane }

{-
type alias TileLayer =
  { url : String
  , size: Size -- need to compute how many / which tiles to show
  --, origin : Position
  , origin : Point -- a projected point on a two-dimensional plane that shares an origin with LatLng
  , levels : Dict Zoom Level
  , crs : CRS
  , currentZoom : Zoom }
-}


makePane : Size -> Maybe String -> InitialCoords -> Zoom -> MapPane
makePane s tu ic zoom = 
    let origin = getInitialCenter ic
        level = 
        initialLevel = Dict.insert zoom level Dict.empty
        tl = 
          Maybe.map (\url -> 
              { url = url, size = s, origin = , levels = Dict.empty, crs=espg4326, currentZoom = 10 } tu
    in
      { dragstate = Nothing
      , size = s
      , position = emptyPos -- the origin of the pane, relative to the origin of the map, in pixels
      , latLngCenter = origin
      , tileLayers = catMaybe [tl] 
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
