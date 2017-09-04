module TileLayer exposing (..)

import Html

import Html exposing (..)
import Html.Attributes exposing (..)
import Regex exposing (Regex, regex, replace, HowMany(..), Match)
import Dict exposing (Dict)
import Set exposing (Set)

import Layer exposing (..)
import Geo exposing (..)
import Util exposing (px, catMaybe, range, zip)

type TileLayerAction
  = TileLayer_Move Position
  | TileLayer_Zoom ZoomDir

type alias TileAddress = (Int, Int, Int)

incZoom : ZoomDir -> Zoom -> Zoom
incZoom zd z =  case zd of
    In -> z + 1
    Out -> z - 1

type alias Tile =
  { url : String 
  , current: Bool
  , address : TileAddress
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

moveTileLayer : Position -> TileLayer -> TileLayer
moveTileLayer pos tl = 
  let pointOrigin = latLngToPoint tl.crs tl.currentZoom tl.latLngOrigin
      level = Maybe.withDefault (createLevel tl.currentZoom) <| Dict.get tl.currentZoom tl.levels 
      newLevel = updateLevel tl.crs tl.urlTemplate tl.currentZoom tl.size pointOrigin pos level
  in 
    { tl | levels = Dict.insert tl.currentZoom newLevel tl.levels }

updateLevel : CRS -> String -> Zoom -> Size -> Point -> Position -> Level -> Level
updateLevel crs temp z paneSize pointOrigin pos level =
      -- Use pixel origin, ie, nw of window, rather than sw
  let pixelOrigin = sum pointOrigin {x=0, y=toFloat -paneSize.y}
      newOrigin = sum (mapCoord (negate << toFloat) pos) pixelOrigin
      tr = getTileRange crs z paneSize newOrigin
      ts = makeTilesFromBounds temp paneSize z pixelOrigin tr
  in 
    { zoom = z, tiles = updateDictWithTiles ts level.tiles }

makeTilesFromBounds : String -> Size -> Zoom -> Point -> Bounds Position ->  List Tile
makeTilesFromBounds temp size z pointOrigin tr = 
      let pairs = List.concat <| List.map (\x -> List.map (\y -> (x,y) ) (range tr.sw.y tr.ne.y) ) (range tr.sw.x tr.ne.x)
      in  List.map (\(x,y) -> createTile temp size z pointOrigin {x=x, y=y}) pairs

updateDictWithTiles : List Tile -> Dict TileKey Tile -> Dict TileKey Tile
updateDictWithTiles tiles dict = 
    let getKey {x,y} = String.join ":" <| List.map toString [x, y]
        nd = Dict.fromList <| List.map (\t -> (getKey(t.position) ,t)) tiles
    in Dict.union dict nd

tileSize = 
  { x = 256
  , y = 256 }

getTileRange : CRS -> Zoom -> Size -> Point -> Bounds Position
getTileRange crs zoom size point = 
  let sf = mapCoord toFloat size
      bs = {sw = point, ne= sum point sf }
  in mapBounds (mapCoord floor << (\co -> quotient co tileSize) ) bs

     
createTile : String -> Size -> Zoom -> Point -> Position -> Tile
createTile temp size zoom pointOrigin tilepos = 
  -- position, in pixels, compared with world origin
  let pos = getPositionFromOrigin pointOrigin tilepos
  in 
    { url = makeUrl {x=tilepos.x, y=tilepos.y, z=zoom } temp
    , current = True
    , address = (tilepos.x, tilepos.y, zoom)
    , position = pos }

getPositionFromOrigin : Point -> Position -> Position
getPositionFromOrigin pointOrigin tilepos = 
  let np = product tileSize <| mapCoord toFloat tilepos
  in mapCoord round <| difference np pointOrigin
 

updateTileLayer : TileLayerAction -> TileLayer -> TileLayer
updateTileLayer tla tl = 
  case tla of
    TileLayer_Move pos -> moveTileLayer pos tl
    TileLayer_Zoom zd ->  
      let nz = incZoom zd tl.currentZoom
          center = getCenterFromOrigin tl.crs tl.currentZoom tl.size <| Debug.log "OO" <| tl.latLngOrigin
          newOrigin = getOriginFromCenter tl.crs nz tl.size center 
          nt = 
            { tl | 
              currentZoom = nz
            , latLngOrigin = newOrigin }
      in moveTileLayer {x=0,y=0} nt


createLevel : Zoom -> Level
createLevel z =
  { zoom = z
  , tiles = Dict.empty  }
  --, active = True }

getDefault : v -> comparable -> Dict comparable v -> v
getDefault def k d = 
  case Dict.get k d of
    Just v -> v
    Nothing -> def

viewTileLevel : Level -> Html a
viewTileLevel level = 
  div
    [ style
        [ ("height", "100%")
        , ("width", "100%")
        , ("position", "absolute")
        ]
    , attribute "data-foliage-name" "tile-level-view"
    ]
    (List.map viewTile <| Dict.values level.tiles)
       

viewTileLayer : TileLayer -> Html a
viewTileLayer tl = 
    let level = getDefault (createLevel tl.currentZoom ) tl.currentZoom tl.levels
    in 
      div
        [ style
          [ ("height", "100%")
          , ("width", "100%")
          , ("position", "absolute")
          ]
          , attribute "data-foliage-name" "tile-layer-view"
        ]
        <| [viewTileLevel level]

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

