module TileLayer exposing (..)

import Html

import Html exposing (..)
import Html.Attributes exposing (..)
import Regex exposing (Regex, regex, replace, HowMany(..), Match)
import Dict exposing (Dict)


--

import Geo exposing (..)
import Util exposing (px, catMaybe, range, zip)

type TileLayerAction
  = TileLayer_Move Position
  | TileLayer_Zoom Zoom
  --| TileLayer_Tile ()


-- To render a tile, we need a url and a position
type alias Tile =
  { url : String 
  , current: Bool
  , position: Position }

tempReg : Regex
tempReg = regex "{([a-z])}"

-- Allow for not replacing anything
replacer : TileSpec -> Match -> String
replacer ts m = 
  case List.head <| catMaybe m.submatches of
    Just s ->  
      case s of
        "x" ->  toString ts.x
        "y" ->  toString ts.y
        "z" ->  toString ts.z
        _  ->  m.match
    Nothing -> Debug.crash "Unrecognized"


makeUrl : TileSpec-> String -> String
makeUrl = replace All tempReg << replacer 

type alias TileKey = String

type alias Level = 
  { origin : Point
  , zoom : Zoom
  , tiles : Dict TileKey Tile }

type alias TileLayer =
  { urlTemplate : String
  , size: Size -- need to compute how many / which tiles to show
  --, origin : Position
  , latLngOrigin : LatLng
  -- a projected point on a two-dimensional plane that shares an origin with LatLng
  , levels : Dict Zoom Level
  , crs : CRS
  , currentZoom : Zoom }


{-|
Updates the position of a TileLayer
-}
moveLayer : Position -> TileLayer -> TileLayer
moveLayer pos tl =
  let pointOrigin = latLngToPoint tl.crs tl.currentZoom tl.latLngOrigin
      pixelBounds = getTiledPixelBounds tl.currentZoom tl.size pointOrigin 
      tileRange = pixelBoundsToTileRange tileSize pixelBounds
      ys = range tileRange.sw.y tileRange.ne.y
      xs = range tileRange.sw.x tileRange.ne.x
      points = List.concat <| List.map (\x -> List.map (\y -> {x = x, y = y} ) ys ) xs
      tiles = Dict.fromList <| List.map (\p -> (tileCoordsToKey p, createTile tl.urlTemplate tl.currentZoom p)) <| Debug.log "DDDDD" points
--- 
      level = Maybe.withDefault (createLevel tl.currentZoom pointOrigin) <| Dict.get tl.currentZoom tl.levels 
      newLevel = { level |
        tiles = Dict.union level.tiles tiles }
  in { tl |
        levels = Dict.insert tl.currentZoom newLevel tl.levels }
        -- latLngCenter = newCenter  }

moveLayer2 : Position -> TileLayer -> TileLayer
moveLayer2 pos tl = 
  let pointOrigin = latLngToPoint tl.crs tl.currentZoom tl.latLngOrigin
      effectiveOrigin = sum pointOrigin <| mapCoord toFloat pos
      level = Maybe.withDefault (createLevel tl.currentZoom pointOrigin) <| Dict.get tl.currentZoom tl.levels 
      newLevel = updateLevel tl.crs tl.urlTemplate tl.currentZoom tl.size pointOrigin effectiveOrigin level
  in 
    { tl |
        levels = Dict.insert tl.currentZoom newLevel tl.levels } 

getLocalPosition : Position -> Size -> Position -> Position
getLocalPosition origin tilesize coords = 
  difference (product coords tilesize) origin

updateLevel : CRS -> String -> Zoom -> Size -> Point -> Point -> Level -> Level
updateLevel crs temp z paneSize pointOrigin newOrigin level =
-- everything changes on level, but we might want to recycle some tiels
  let pixelBounds = getTiledPixelBounds z paneSize newOrigin
      tr = pixelBoundsToTileRange tileSize pixelBounds
      --pairs = List.map (\x -> List.map (\y -> {x = x, y = y} ) ys ) xs
      pairs = List.concat <| List.map (\x -> List.map (\y -> (x,y) ) (range tr.sw.y tr.ne.y) ) (range tr.sw.x tr.ne.x)
-- Debug.log "pairs" <| zip (range tr.sw.x tr.ne.x) (range tr.sw.y tr.ne.y)
      ts =  Debug.log "FOO" <| List.map (\(x,y) -> createTile2 temp pointOrigin z {x=x, y=y}) pairs
      newTiles = Dict.fromList <| List.map (\t -> (tileCoordsToKey t.position, t)) ts
  in 
    { zoom = z
    , origin = pointOrigin -- Needed?
    , tiles = Dict.union newTiles level.tiles  }

createTile2 : String -> Point -> Zoom -> Position -> Tile
createTile2 temp pointOrigin zoom tilepos = 
  { url = makeUrl {x=tilepos.x, y=tilepos.y, z=zoom } temp
  , current = True
  , position = getLocalPosition (mapCoord round pointOrigin) tileSize tilepos }

getTiledPixelBounds : Zoom -> Size -> Point -> Bounds Point
getTiledPixelBounds zoom mapSize coords = 
  --let center = latLngToPoint crs zoom coords
  let half = mapCoord (\n -> (toFloat n) / (2 * (scaleZoom zoom))) mapSize
  in
    { sw = difference coords half, ne = sum coords half }

tileCoordsToKey : Position -> TileKey
tileCoordsToKey p = String.join ":" <| List.map toString [p.x, p.y]


createTile : String -> Zoom -> Position -> Tile
createTile temp zoom pos =
  { url = makeUrl {x=pos.x, y=pos.y, z=zoom} temp
  , current = True
  , position = pos }

pixelBoundsToTileRange : Size -> Bounds Point -> Bounds Position
pixelBoundsToTileRange size bounds =
    let floatSize = mapCoord toFloat size
    in
      { sw = mapCoord floor <| quotient bounds.sw floatSize
      , ne = mapCoord ceiling <| quotient bounds.ne floatSize }

tileSize = 
  { x = 256
  , y = 256 }


-- moveLayer2 : Position -> TileLayer -> TileLayer
updateTileLayer : TileLayerAction -> TileLayer -> TileLayer
-- (TileLayer, Cmd TileLayerAction)
updateTileLayer tla tl = 
  case tla of
    TileLayer_Move pos -> moveLayer2 pos tl
    TileLayer_Zoom zoom ->  Debug.crash "FOO"
    --_ -> (tl, Cmd.none)

-- updateLevel : CRS -> String -> Zoom -> Size -> Point -> Level -> Level

createLevel : Zoom -> Point -> Level
createLevel z or =
  { zoom = z
  , origin = or 
  , tiles = Dict.empty }

getDefault : v -> comparable -> Dict comparable v -> v
getDefault def k d = 
  case Dict.get k d of
    Just v -> v
    Nothing -> def

viewTileLayer : TileLayer -> Html a
viewTileLayer tl = 
    let pointOrigin = latLngToPoint tl.crs tl.currentZoom tl.latLngOrigin
        level = getDefault (createLevel tl.currentZoom pointOrigin) tl.currentZoom tl.levels
    in 
      div
        [ style
          [ ("height", "100%")
          , ("width", "100%")
          ]
        ]
        ( List.map viewTile <| Dict.values <| level.tiles)

viewTile : Tile -> Html a
viewTile t = 
      img
        [ style
            [ ("left", px t.position.x)
            , ("top", px t.position.y)
            , ("height", px 256)
            , ("width", px 256)
            , ("pointer-events", "none")
            , ("position", "absolute")
            ],
          src t.url
        ] 
        [ ]



