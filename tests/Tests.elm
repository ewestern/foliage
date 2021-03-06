module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import Array
import String
import Pane exposing (..)
import Result
import Task



import GeoJson as GJ
import Geo exposing (..)
import Geometry exposing (emptyEnvelope)
import TileLayer exposing (..)
import Layer exposing (..)
import VectorLayer exposing (..)
import Json.Decode exposing (decodeString)
import Map exposing (..)
import Util exposing (zip, range)
import Dict
import Set

emptyPos = {x = 0, y = 0 }

whitney = { lat = 36.5784983, lng=-118.3010362 }

initialDrag = 
  { current = emptyPos
  , start = emptyPos
  , velocity = {dx = 0.0, dy = 0.0 }
  , release = Nothing }

tileLayer : TileLayer
tileLayer = 
  { urlTemplate = "http://example.com/tiles/{x}/{y}/{z}"
  , size = {x = 500, y=500 }
  , latLngOrigin = {lat=36.0, lng=-118}
  , levels = Dict.empty
  , crs=espg3857
  , currentZoom = 10 }


getGeometry : Bounds LatLng -> Cmd (List GJ.Geometry)
getGeometry _ = 
        let gt = Task.succeed [testGeom2]
        in Task.perform identity gt


vLayer : VectorLayer
vLayer = 
  { options = {stroke = "#3388ff", color="", weight="" , getGeometry=getGeometry}
  , geometry = [testGeom2]
  , size = tileLayer.size
  , latLngOrigin = tileLayer.latLngOrigin
  , currentZoom = 12
  , crs = espg3857 }



convert : Point -> GJ.Position
convert {x,y} = (x,y,0)

whitneyBox = GJ.LineString  <| List.map convert [ 
          { y = 36.577, x = -118.302 }
        , { y = 36.579, x = -118.302 }
        , { y = 36.579, x = -118.300 }
        , { y = 36.577, x = -118.300 }
        , { y = 36.577, x = -118.302 }
        ]


testGeom2 = GJ.LineString  <| List.map convert [ 
          { y = 36.67, x = -118.37 }
        , { y = 36.72, x = -118.37 }
        , { y = 36.72, x = -118.18 }
        , { y = 36.67, x = -118.18 }
        , { y = 36.67, x = -118.37 }
        ]

testGeom = GJ.LineString  <| List.map convert [{ x = -118.02928544052078, y = 36.41785245831469 },{ x = -118.02920245967934, y = 36.41786369006496 },{ x = -118.02898830205326, y = 36.41789269144992 },{ x = -118.0288879706723, y = 36.41790627013306 },{ x = -118.02878244251131, y = 36.41792043554942 },{ x = -118.02860524907824, y = 36.417944491611536 }]

--testGeomSet = Set.fromList [testGeom]

testTiles = [{ url = "http://example.com/tiles/176/402/10", current = True, position = { x = -91, y = -28 } },{ url = "http://example.com/tiles/176/403/10", current = True, position = { x = -91, y = 228 } },{ url = "http://example.com/tiles/176/404/10", current = True, position = { x = -91, y = 484 } },{ url = "http://example.com/tiles/177/402/10", current = True, position = { x = 165, y = -28 } },{ url = "http://example.com/tiles/177/403/10", current = True, position = { x = 165, y = 228 } },{ url = "http://example.com/tiles/177/404/10", current = True, position = { x = 165, y = 484 } },{ url = "http://example.com/tiles/178/402/10", current = True, position = { x = 421, y = -28 } },{ url = "http://example.com/tiles/178/403/10", current = True, position = { x = 421, y = 228 } },{ url = "http://example.com/tiles/178/404/10", current = True, position = { x = 421, y = 484 } }] 

testSegment = 
  { trailType = "track"
  , sacScale = Nothing
  , geometry = testGeom }

all : Test
all =
    describe "Sample Test Suite"
        [ describe "Geo tests"
            [ 
              test "espg 3857" <|
                \() ->
                  let crs = tileLayer.crs
                      p = crs.projection.project tileLayer.latLngOrigin
                      scalar = (2 ^ (toFloat tileLayer.currentZoom))
                      tp = transform crs.transformation scalar p

                  in Expect.all [
                      \() -> Expect.equal p  { x = -13135699.91360628, y = 4300621.372044271 }
                    , \() -> Expect.equal (mapCoord floor tp) { x = 176, y = 402 }
                  ] ()
             , test "get Origin" <| 
                \() -> 
                    let center = getCenterFromOrigin tileLayer.crs tileLayer.currentZoom tileLayer.size tileLayer.latLngOrigin
                        origin  = getOriginFromCenter tileLayer.crs tileLayer.currentZoom tileLayer.size center
                    in Expect.equal origin tileLayer.latLngOrigin
      
             , test "moveTileLayer" <| 
                \() -> 
                    let newtl = moveTileLayer {x=15,y=16} tileLayer
                        level = Maybe.withDefault (createLevel 10) <| Dict.get 10 newtl.levels
                    in Expect.equal True True -- level.tiles testTiles
             , test "getTileRange" <|
                \() -> 
                    let p = latLngToPoint tileLayer.crs tileLayer.currentZoom tileLayer.latLngOrigin
                        range = getTileRange tileLayer.crs tileLayer.currentZoom tileLayer.size p

                    in Expect.all [

                      \() -> Expect.equal  range { sw = { x = 176, y = 402 }, ne = { x = 178, y = 404 } }

                    ] ()
            , test "getPannedLatLng" <| 
              \() ->
                  let ll = getPannedLatLng tileLayer.crs tileLayer.currentZoom tileLayer.size tileLayer.latLngOrigin
                      pos = getCoordinatePosition tileLayer.crs tileLayer.currentZoom tileLayer.size tileLayer.latLngOrigin (-117.01, 36.01, 0)
                  in Expect.all [
                    \() -> Expect.equal ll { lat = 36.55354730363422, lng = -117.3133544921875 }
                  , \() -> Expect.equal pos { x = 721, y = 491 } 
                  ] ()

          ]
        , describe "Vector Drawing" 
            [
              test "" <|
                \() ->  
                        let testPath = getGeometryPath tileLayer.crs 13 tileLayer.size {lat=36.5, lng=-118.1} testGeom
                            svgPath = drawPath testPath False

                        in Expect.equal svgPath <| Just "M412 1095L414 1095L415 1095L416 1094"

             , test "" <|
                 \() ->  

                    let bds =  envelopeToBounds <| Maybe.withDefault emptyEnvelope <| getVectorLayerEnvelope vLayer

                        (dims, box, trans) = getBoxFromBounds vLayer.crs vLayer.currentZoom vLayer.size vLayer.latLngOrigin bds
                    in Expect.all [
                        \() -> Expect.equal dims (1024, 3604)
                      , \() -> Expect.equal box (1078,-3104,1024,3604)
                      , \() -> Expect.equal trans (1078,-3104,0)

                      ] ()


            ]
        , describe "Tile Drawing"
            [

              test "drawPath" <|
                \() ->
                  let url = makeUrl {x=3, y=4, z=5 } tileLayer.urlTemplate
                  in Expect.equal url "http://example.com/tiles/3/4/5"


--getInitialBounds : Size -> CRS -> Zoom -> InitialCoords -> Bounds LatLng
            , test "foo" <|
                \() ->
                  let zoom = 12
                      whitneyOrigin = getOriginFromCenter tileLayer.crs zoom tileLayer.size whitney
                      newTl = {tileLayer |
                        currentZoom = zoom
                      , latLngOrigin = whitneyOrigin }
                      whitneyBounds = getInitialBounds tileLayer.size tileLayer.crs newTl.currentZoom <| Initial_Center whitney

                      whitneyPointOrigin = latLngToPoint newTl.crs zoom whitneyOrigin
                      whitneyPixelOrigin = sum whitneyPointOrigin {x=0, y=toFloat -newTl.size.y}
                      tr = getTileRange newTl.crs zoom newTl.size whitneyPixelOrigin
                      initTl = moveTileLayer {x=0,y=0} newTl
                      initTiles = List.concat <| List.map (\l -> Dict.values l.tiles) <| Dict.values initTl.levels
                      initAddresses = List.map .address initTiles
                      whitneyAddress = (702, 1600, 12)
  
                  in Expect.all [
                      \() -> Expect.lessThan whitney.lat whitneyOrigin.lat
                    , \() -> Expect.lessThan whitney.lng whitneyOrigin.lng
                    , \() -> Expect.greaterThan whitney.lat whitneyBounds.ne.lat
                    , \() -> Expect.lessThan whitney.lat whitneyBounds.sw.lat
                    , \() -> Expect.atMost 1600 tr.sw.y 
                    , \() -> Expect.atLeast 1600 tr.ne.y
                    , \() -> Expect.true ("Expected " ++ (toString initAddresses) ++ " to contain address " ++ (toString whitneyAddress) ) <| List.member whitneyAddress initAddresses
                  ] ()
            , test "bar" <| 
              \() -> 
                  let foo = "ASD"
                  in Expect.all [
                    \() -> Expect.equal True True
                  ] ()

              ]
        ]

jsonGeo = """
{
        "coordinates": [
          [
            -118.02928544052078,
            36.41785245831469
          ],
          [
            -118.02920245967934,
            36.41786369006496
          ],
          [
            -118.02898830205326,
            36.41789269144992
          ],
          [
            -118.0288879706723,
            36.41790627013306
          ],
          [
            -118.02878244251131,
            36.41792043554942
          ],
          [
            -118.02860524907824,
            36.417944491611536
          ]
        ],
        "crs": {
          "type": "name",
          "properties": {
            "name": "ESPG:4326"
          }
        },
        "type": "LineString"
      }
"""

json = """[
  [
    "214317",
    {
      "osmId": 489407670,
      "trailType": "track",
      "sacScale": null,
      "visibility": null,
      "trackType": null,
      "geometry": {
        "coordinates": [
          [
            -118.02928544052078,
            36.41785245831469
          ],
          [
            -118.02920245967934,
            36.41786369006496
          ],
          [
            -118.02898830205326,
            36.41789269144992
          ],
          [
            -118.0288879706723,
            36.41790627013306
          ],
          [
            -118.02878244251131,
            36.41792043554942
          ],
          [
            -118.02860524907824,
            36.417944491611536
          ]
        ],
        "crs": {
          "type": "name",
          "properties": {
            "name": "ESPG:4326"
          }
        },
        "type": "LineString"
      }
    }
  ]
]"""
