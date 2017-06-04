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
  { zoom : Zoom
  --, origin : Point
  , tiles : Dict TileKey Tile }

type alias TileLayer =
  { urlTemplate : String
  , size: Size -- need to compute how many / which tiles to show
  , latLngOrigin : LatLng -- represents SW corner of map
  , levels : Dict Zoom Level
  , crs : CRS
  , currentZoom : Zoom }


{-|
Updates the position of a TileLayer
-}

moveLayer : Position -> TileLayer -> TileLayer
moveLayer pos tl = 
  let pointOrigin = tl.crs.projection.project tl.latLngOrigin
      --pointOrigin = latLngToPoint tl.crs tl.currentZoom tl.latLngOrigin
      effectiveOrigin = sum pointOrigin <| mapCoord toFloat pos
      level = Maybe.withDefault (createLevel tl.currentZoom) <| Dict.get tl.currentZoom tl.levels 
      newLevel = updateLevel tl.crs tl.urlTemplate tl.currentZoom tl.size pointOrigin effectiveOrigin level
  in 
    { tl |
        levels = Dict.insert tl.currentZoom newLevel tl.levels } 

getLocalPosition : Position -> Size -> Position -> Position
getLocalPosition origin tilesize coords = 
  difference (product coords tilesize) origin


getTileName : CRS -> Zoom -> Point -> Position
getTileName crs zoom point  = 
  let scalar = 2 ^ (toFloat zoom)
      p = transform crs.transformation scalar point
      -- transform the point such that, instead of represention a point on earth, it represents a "point" on a grid (2^zoom) x (2^zoom) in size
  in mapCoord floor p


getTileRange : CRS -> Zoom -> Size -> Point -> Bounds Position
getTileRange crs zoom size point = 
  let scalar = 2 ^ zoom
      or = transform crs.transformation (toFloat scalar) point
      ratio = quotient (mapCoord toFloat size) (mapCoord toFloat tileSize)
      ne = sum or ratio
  in { sw = mapCoord floor or, ne = mapCoord floor ne }
    

updateLevel : CRS -> String -> Zoom -> Size -> Point -> Point -> Level -> Level
updateLevel crs temp z paneSize pointOrigin newOrigin level =
  let tr = getTileRange crs z paneSize newOrigin
      pairs = List.concat <| List.map (\x -> List.map (\y -> (x,y) ) (range tr.sw.y tr.ne.y) ) (range tr.sw.x tr.ne.x)
      ts =  List.map (\(x,y) -> createTile temp pointOrigin z {x=x, y=y}) pairs
      newTiles = Dict.fromList <| List.map (\t -> (tileCoordsToKey t.position, t)) ts
  in 
    { zoom = z, tiles = Dict.union newTiles level.tiles  }

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

createLevel : Zoom -> Level
createLevel z =
  { zoom = z
  , tiles = Dict.empty }

getDefault : v -> comparable -> Dict comparable v -> v
getDefault def k d = 
  case Dict.get k d of
    Just v -> v
    Nothing -> def

viewTileLayer : TileLayer -> Html a
viewTileLayer tl = 
    let level = getDefault (createLevel tl.currentZoom ) tl.currentZoom tl.levels
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

