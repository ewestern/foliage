module Map exposing (mapSubscriptions, updateMap, mapView, Map, InitialCoords(..), MapAction(..), MapOptions, getInitialBounds)

import Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onWithOptions, defaultOptions)
import Svg exposing (rect, svg)
import Svg.Attributes  as SA
import Json.Decode as Decode

import Dict
import Mouse
---
import Pane exposing (..)
import Geo exposing (..)
import Util exposing (..)
import TileLayer exposing (TileLayer, updateTileLayer, TileLayerAction(..))
import VectorLayer exposing (VectorLayer, VectorLayerOptions)
import Layer exposing (pointToLatLng, latLngToPoint, getBounds, getOriginFromCenter, ZoomDir(..))


type alias Map
  = { size : Size
    ,  pane : MapPane }

type InitialCoords
  = Initial_Center LatLng
  | Initial_Bounds (Bounds LatLng)

type MapAction
    = Map_Pane PaneAction
    | Map_VectorLayer VectorLayerOptions
    | Map_TileLayer TileLayerOptions

-- tileUrl : Maybe String
type alias MapOptions =
  { initialCoords : InitialCoords
  --, vectorOptions : List VectorOptions
  --, tileOptions : List TileOptions
  , initialZoom : Zoom
  , crs : CRS
  , size : Size }
  
defaultOptions : MapOptions
defaultOptions
  { size = {x=500, y=500}
  , initialCoords = {lat=51.476, lng=0}
  , crs = espg3857
  , tileOptions = []
  , vectorOptions = [] }

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
    in
      { dragstate = Nothing
      , size = mo.size
      , position = emptyPos 
      , tileLayers = List.map (updateTileLayer (TileLayer_Move {x=0, y=0})) <| catMaybe [initialLayer] 
      , vectorLayers = [mkVectorLayer mo.vectorOptions mo.crs mo.size sw mo.initialZoom] }

{-| The map's view function
-}
mapView : Map -> Html.Html MapAction
mapView  map =  
      div
        [ style
            [ ("height", px map.size.y)
            , ("width", px map.size.x)
            , ("position", "relative")
            , ("overflow", "hidden")
            ]
        ] 
        [ Html.map Map_Pane <| viewPane map.pane 
        , Html.map (Map_Pane << Pane_Zoom) <| zoomContainer {x=10, y=10}  ]
zoomContainer : Position -> Html ZoomDir
zoomContainer pos =
    div
      [ style
        [ ("border", "2px solid rgba(0,0,0,0.2)")
        , ("margin-left", px pos.x)
        , ("margin-top", px pos.y)
        , ("float", "left")
        , ("clear", "both")
        , ("z-index", "1000")
        , ("position", "relative")
        , ("cursor", "auto")
        ]
      ]
    [ zoomButtonView In, zoomButtonView Out ]
    
onMouseDownQuiet : (Position -> msg) -> Attribute msg
onMouseDownQuiet f = 
  let opts = { defaultOptions | stopPropagation = True }
  in onWithOptions "mousedown" opts (Decode.map f Mouse.position )


zoomButtonView : ZoomDir -> Html ZoomDir
zoomButtonView s = 
      a
        [ onMouseDownQuiet <| always s
        , attribute "role" "button"
        , attribute "href" "#"
        , style
            [ ("height", "30px")
            , ("width", "30px")
            , ("font-size", "22px")
            , ("text-align", "center")
            , ("display", "block")
            , ("background-color", "#fff")
            , ("text-decoration", "none")
            , ("-webkit-tap-highlight-color",  "transparent")
            ]
        ]
        [zoomIconView s]

zoomIconView : ZoomDir -> Html a
zoomIconView z = case z of
    In -> text "+"
    Out -> text "-"
    

icon : Html a
icon = 
  let r = rect [ SA.width "40", SA.height "40"] []
  in svg [] [r]


{-| The map's main update function

-}
updateMap : MapAction -> Map -> (Map, Cmd MapAction)
updateMap action map = 
  case action of
    Map_Pane da -> 
      let (mp, cd) = updatePane da map.pane
          nm = { map | pane = mp }
          cmd  = Cmd.map Map_Pane cd
      in (nm, cmd)

{-| A default set of subscriptions that must be listened for.
-}
mapSubscriptions  : Map -> Sub MapAction
mapSubscriptions map = 
     Sub.map (Map_Pane << Pane_Drag) <| dragSubscription map.pane.dragstate 
