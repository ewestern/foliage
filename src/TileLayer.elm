module TileLayer exposing (..)

import Html

import Html exposing (..)
import Html.Attributes exposing (..)
import Regex exposing (Regex, regex, replace, HowMany(..), Match)
import Mouse exposing (Position)
import Dict exposing (Dict)


--

import Geo exposing (..)
import Util exposing (px, catMaybe)


-- To render a tile, we need a url and a position
type alias Tile =
  { url : String 
  , position: Position }

type TileAction = 
    Empty

type alias TileSpec = 
  { x : Int
  , y : Int
  , z : Int }

-- 

tempReg : Regex
tempReg = regex "/{[a-z]}/"

replacer : TileSpec -> Match -> String
replacer ts m = 
  case List.head <| catMaybe m.submatches of
    Just s ->  
      case s of
        "x" ->  toString ts.x
        "y" ->  toString ts.y
        "z" ->  toString ts.z
        _   ->  m.match
    Nothing -> Debug.crash "Improperly formatted url template"



makeUrl : TileSpec-> String -> String
makeUrl ts temp = replace All tempReg (replacer ts) temp

-- <img alt="" role="presentation" src="http://b.tile.openstreetmap.org/14/8185/5448.png" class="leaflet-tile leaflet-tile-loaded" style="width: 256px; height: 256px; transform: translate3d(276px, 5px, 0px); opacity: 1;">
viewTile : Tile -> Html TileAction
viewTile t = 
      img
        [ style
            --[ ("left", px pane.position.x)
            --, ("top", px pane.position.y)
            [ ("height", px 256)
            , ("width", px 256)
            , ("src", t.url)
            , ("position", "absolute")
            ]
        ] 
        [ ]

-- given a tile layer
type alias Zoom = Int
type alias TileKey = String

type alias Level = 
  { origin : Point
  , zoom : Zoom
  , tiles : Dict TileKey Tile }

type alias TileLayer =
  { url : String
  , size: Size -- need to compute how many / which tiles to show
  --, origin : Position
  , origin : Point -- a projected point on a two-dimensional plane that shares an origin with LatLng
  , levels : Dict Zoom Level
  , crs : CRS
  , currentZoom : Zoom }


{-
 _getTilePos: function (coords) {
    return coords.scaleBy(this.getTileSize()).subtract(this._level.origin);
  },
-}
----
{-
FROM OLD
moveLayer :  Point -> TileLayer -> TileLayer
moveLayer moveTo tl = 
  -- get the vector in pixel terms (from geographical origin, not layer origin)
  let newCenter = getCenter tl moveTo
      pixelBounds = getTiledPixelBounds tl.crs tl.currentZoom tl.mapSize newCenter 
      tileRange = Debug.log "tileRange" <| pixelBoundsToTileRange pixelBounds
      ys = range tileRange.sw.y tileRange.ne.y
      xs = range tileRange.sw.x tileRange.ne.x
      points = List.concat <| List.map (\x -> List.map (\y -> {x = x, y = y} ) ys ) xs
      tiles = Debug.log "tiles" <| Dict.fromList <| List.map (\p -> (tileCoordsToKey p,createTile p)) points
      level = Maybe.withDefault (createLevel tl.currentZoom tl.pixelOrigin) <| Dict.get tl.currentZoom tl.levels 
      newLevel = { level |
        tiles = Dict.union level.tiles tiles }
  in { tl |
        levels = Dict.insert tl.currentZoom newLevel tl.levels,
        latLngCenter = newCenter  }
-}

