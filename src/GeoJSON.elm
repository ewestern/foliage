module GeoJSON exposing (..)

import Types exposing (..)

import Array exposing (fromList, map)
import Json.Decode as D
--(Decoder, map2)
import Json.Encode exposing (Value, string, array, object, float)

coordinateValue : Coordinate -> Value
coordinateValue {x,y} = array <| fromList <| List.map float [x,y]

pointToGeoJSON : Point -> Value
pointToGeoJSON (Point p) = 
  object  [ ( "type", string "Point")
          , ( "coordinates", coordinateValue p) ]

lineStringToGeoJSON : LineString -> Value
lineStringToGeoJSON (LineString arr)  = 
  object  [ ("type", string "LineString")
          , ("coordinates", array <| map coordinateValue arr) ]

linearRingValue : LinearRing -> Value
linearRingValue lr = array <| map coordinateValue lr

polygonToGeoJSON : Polygon -> Value
polygonToGeoJSON (Polygon pg) = 
  object  [ ("type", string "Polygon")
          , ("coordinates", array <| map linearRingValue pg) ]
  
jsonType : D.Decoder String
jsonType = D.field "type" D.string

decodeGeometry : D.Decoder Geometry
decodeGeometry = D.andThen decodeGeometryHelper jsonType

decodeGeometryHelper : String -> D.Decoder Geometry
decodeGeometryHelper s =
  case s of 
    "Point"       -> D.map Geometry_Point decodePoint
    "LineString"  -> D.map Geometry_LineString decodeLineString
    "Polygon"     -> D.map Geometry_Polygon decodePolygon
    _             -> D.fail "Unrecognized Geometry Type"

decodePoint : D.Decoder Point
decodePoint = D.map Point (D.field "coordinates" decodeCoordinate)

decodeCoordinate : D.Decoder Coordinate
decodeCoordinate = 
  D.map2 Coordinate
    (D.index 0 D.float)
    (D.index 1 D.float)

decodeLinearRing : D.Decoder LinearRing
decodeLinearRing =
  D.array decodeCoordinate

decodeLineString : D.Decoder LineString
decodeLineString = D.map LineString <| D.field "coordinates" decodeLinearRing

decodePolygon : D.Decoder Polygon
decodePolygon = D.map Polygon <| D.field "coordinates" <| D.array decodeLinearRing



--geometryDecoder : String -> 
