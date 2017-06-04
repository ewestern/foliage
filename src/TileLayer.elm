module TileLayer exposing (..)

import Html

import Html exposing (..)
import Html.Attributes exposing (..)
import Regex exposing (Regex, regex, replace, HowMany(..), Match)
import Dict exposing (Dict)

import Geo exposing (..)
import Util exposing (px, catMaybe, range, zip)

type TileLayerAction
  = TileLayer_Move Position
  | TileLayer_Zoom ZoomDir

type ZoomDir
  = In
  | Out

incZoom : ZoomDir -> Zoom -> Zoom
incZoom zd z =  case zd of
    In -> z + 1
    Out -> z - 1

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
  , latLngOrigin : LatLng -- represents SW corner of map
  , levels : Dict Zoom Level
  , crs : CRS
  , currentZoom : Zoom }


{-|
Updates the position of a TileLayer
-}

moveLayer : Position -> TileLayer -> TileLayer
moveLayer pos tl = 
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


getTileName : Point -> Zoom -> Position
getTileName point zoom = 
  let n = toFloat <| 2 ^ zoom
  --let n = toFloat <| 2 ^ zoom
      --x  = (1 + (point.x / pi) ) / 2
      --y = (1 - (point.y / pi) ) / 2
  in { x = floor <| point.x / n , y = floor <| point.y / n }


getTileRange : Point -> Size -> Zoom -> Bounds Position
getTileRange point paneSize zoom = 
  let ne = sum (mapCoord toFloat paneSize) point
  in { sw = getTileName point zoom, ne = getTileName ne zoom }

pixelBoundsToTileRange : Size -> Bounds Point -> Bounds Position
pixelBoundsToTileRange size bounds =
    let floatSize = mapCoord toFloat size
    in
      { sw = mapCoord floor <| quotient bounds.sw floatSize
      , ne = mapCoord ceiling <| quotient bounds.ne floatSize }

getTiledPixelBounds : Zoom -> Size -> Point -> Bounds Point
getTiledPixelBounds zoom mapSize coords = 
-- TODO: this overestimates size
  let size = mapCoord toFloat mapSize
-- mapCoord (\n -> (toFloat n) / scaleZoom zoom) mapSize
  --let half = mapCoord (\n -> (toFloat n) / scaleZoom zoom) mapSize
  in
    { sw = coords, ne = sum coords size }



updateLevel : CRS -> String -> Zoom -> Size -> Point -> Point -> Level -> Level
updateLevel crs temp z paneSize pointOrigin newOrigin level =
-- everything changes on level, but we might want to recycle some tiels
  let pixelBounds = getTiledPixelBounds z paneSize <| Debug.log "OR" newOrigin
      tr = Debug.log "Range" <| pixelBoundsToTileRange tileSize <| Debug.log "Bounds" pixelBounds
  --let tr = getTileRange newOrigin paneSize z
      pairs = List.concat <| List.map (\x -> List.map (\y -> (x,y) ) (range tr.sw.y tr.ne.y) ) (range tr.sw.x tr.ne.x)
-- Debug.log "pairs" <| zip (range tr.sw.x tr.ne.x) (range tr.sw.y tr.ne.y)
      ts =  Debug.log "FOO" <| List.map (\(x,y) -> createTile temp pointOrigin z {x=x, y=y}) pairs
      newTiles = Dict.fromList <| List.map (\t -> (tileCoordsToKey t.position, t)) ts
  in 
    { zoom = z
    , origin = pointOrigin -- Needed?
    , tiles = Dict.union newTiles level.tiles  }

createTile : String -> Point -> Zoom -> Position -> Tile
createTile temp pointOrigin zoom tilepos = 
  { url = makeUrl {x=tilepos.x, y=tilepos.y, z=zoom } temp
  , current = True
  , position = getLocalPosition (mapCoord round pointOrigin) tileSize tilepos }


tileCoordsToKey : Position -> TileKey
tileCoordsToKey p = String.join ":" <| List.map toString [p.x, p.y]


tileSize = 
  { x = 256
  , y = 256 }


updateTileLayer : TileLayerAction -> TileLayer -> TileLayer
updateTileLayer tla tl = 
  case tla of
    TileLayer_Move pos -> moveLayer pos tl
    TileLayer_Zoom zd ->  
      let nz = incZoom zd tl.currentZoom
      in moveLayer {x=0,y=0} { tl | currentZoom = nz }

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

