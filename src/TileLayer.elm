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
  { zoom : Zoom, tiles : Dict TileKey Tile }

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

{-
the Mercator degrees / pixels ratio will be different at different zooms (and lattitudes, probably). 
if the tile coordinate is the mercator degree scaled to the number of tiles across it's range, then the relative pixel is that range times the tilesize



-}
moveLayer : Position -> TileLayer -> TileLayer
moveLayer pos tl = 
  let pointOrigin = tl.crs.projection.project tl.latLngOrigin
-- get the mercator origin at the current nw cornder of the map
      effectiveOrigin = difference pointOrigin <| doThing tl.crs tl.size tl.currentZoom pos
-- sum pointOrigin <| mapCoord toFloat pos
      level = Maybe.withDefault (createLevel tl.currentZoom) <| Dict.get tl.currentZoom tl.levels 
      newLevel = updateLevel tl.crs tl.urlTemplate tl.currentZoom tl.size pointOrigin effectiveOrigin level
  in 
    { tl | levels = Dict.insert tl.currentZoom newLevel tl.levels }

{-|
With {0,0} representing the origin NW corner, find the coordinate at which to place a tile

-}

-- want a number, such that we can add to pointOrigin (sw corder)
adjustPosition : Size -> Position -> Position
adjustPosition paneSize pos = {x=pos.x, y=paneSize.y - pos.y }


tileNameToPixel : Position -> Position
tileNameToPixel pos = product tileSize pos

pixelToTileName : Position -> Point
pixelToTileName px = quotient (mapCoord toFloat px) (mapCoord toFloat tileSize)

mercatorToPixel : CRS -> Zoom -> Point -> Position
mercatorToPixel crs zoom pnt = 
  let tn = getTileName crs zoom pnt
  in tileNameToPixel tn


doThing : CRS -> Size -> Zoom -> Position -> Point
doThing crs size zoom pos = 
  difference  (pixelToMercator crs zoom pos) (pixelToMercator crs zoom {x=0, y=0})

pixelToMercator : CRS -> Zoom -> Position -> Point
pixelToMercator crs zoom pos = tileNameToPoint crs zoom <| pixelToTileName pos

getLocalPosition : CRS -> Size -> Zoom -> Point -> Position -> Position
getLocalPosition crs size zoom origin tileName = 
    difference (tileNameToPixel tileName) (mercatorToPixel crs zoom origin)


{-|
From a tile name, retrieve a mercator-projected point.

-}
tileNameToPoint : CRS -> Zoom -> Point -> Point
tileNameToPoint crs zoom coord = 
  let scalar = 2 ^ (toFloat zoom)
  in  untransform crs.transformation scalar coord

getTileName : CRS -> Zoom -> Point -> Position
getTileName crs zoom point  = 
  let scalar = 2 ^ (toFloat zoom)
  in mapCoord floor <| transform crs.transformation scalar point
      -- transform the point such that, instead of represention a point on earth, it represents a "point" on a grid (2^zoom) x (2^zoom) in size


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
      ts =  List.map (\(x,y) -> createTile crs paneSize temp pointOrigin z {x=x, y=y}) pairs
      newTiles = Dict.fromList <| List.map (\t -> (tileCoordsToKey t.position, t)) ts
  in 
    { zoom = z, tiles = Dict.union newTiles level.tiles  }

createTile : CRS -> Size -> String -> Point -> Zoom -> Position -> Tile
createTile crs size temp pointOrigin zoom tilepos = 
  { url = makeUrl {x=tilepos.x, y=tilepos.y, z=zoom } temp
  , current = True
  , position = getLocalPosition crs size zoom pointOrigin tilepos }


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

