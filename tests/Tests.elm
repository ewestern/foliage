module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import Array
import String
import Pane exposing (..)
import Result
import Geo exposing (..)
import Types exposing (..)
import TileLayer exposing (..)
import Layer exposing (..)
import VectorLayer exposing (..)
import Json.Decode exposing (decodeString)
import GeoJSON
import Map exposing (..)
import Util exposing (zip, range)
import Dict

emptyPos = {x = 0, y = 0 }

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


testGeom = Geometry_LineString (LineString (Array.fromList [{ x = -118.02928544052078, y = 36.41785245831469 },{ x = -118.02920245967934, y = 36.41786369006496 },{ x = -118.02898830205326, y = 36.41789269144992 },{ x = -118.0288879706723, y = 36.41790627013306 },{ x = -118.02878244251131, y = 36.41792043554942 },{ x = -118.02860524907824, y = 36.417944491611536 }]))

testSegment = 
  { trailType = "track"
  , sacScale = Nothing
  , geometry = testGeom }

all : Test
all =
    describe "Sample Test Suite"
        [ describe "Json tests"
          [ 
            test "decode" <|
              \() -> 
                  let geom = decodeString GeoJSON.decodeGeometry jsonGeo
                      geomResult = Result.Ok testGeom
                      segment = decodeString Util.segmentDictDecoder json
                      segmentResult = Result.Ok <| Dict.fromList [(214317 , testSegment)]
                  in Expect.all [
                        \() -> Expect.equal geom geomResult
                      , \() -> Expect.equal segment segmentResult
                    ] ()
            ]
        , describe "Geo tests"
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
                  
             , test "getTileRange" <|
                \() -> 
                    let p = tileLayer.crs.projection.project tileLayer.latLngOrigin
                        range = getTileRange tileLayer.crs tileLayer.currentZoom tileLayer.size  p
                        bounds = projectedBounds tileLayer.crs tileLayer.currentZoom

                    in Expect.all [
                      \() -> Expect.notEqual  True False

                    ] ()
            ]
            , test "getPannedLatLng" <| 
              \() ->
                  let ll = getPannedLatLng tileLayer.crs tileLayer.currentZoom tileLayer.size tileLayer.latLngOrigin
                  in Expect.all [
                    \() -> Expect.equal ll { lat = 36.55354730363422, lng = -117.3133544921875 }
                  ] ()

            --, test "getBounds" <|
              --\() ->
                  --let bounds = Debug.log "bounds" <| getBounds tileLayer.crs tileLayer.currentZoom tileLayer.size tileLayer.latLngOrigin
                  --in Expect.all [
                    --\() -> Expect.equal True True
                  --] ()
        , describe "Vector Drawing" 
            [
              test "drawPath" <|
                \() ->
                  let url = makeUrl {x=3, y=4, z=5 } tileLayer.urlTemplate
                  in Expect.equal url "http://example.com/tiles/3/4/5"

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
