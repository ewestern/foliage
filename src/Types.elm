module Types exposing (..)
{-
 The coordinate reference system for all GeoJSON coordinates is a
   geographic coordinate reference system, using the World Geodetic
   System 1984 (WGS 84) [WGS84] datum, with longitude and latitude units
   of decimal degrees.  

-}
import Array exposing (Array)


type alias Coordinate = { x: Float, y: Float }
type Point = Point Coordinate

type LineString = LineString (Array Coordinate)

type alias LinearRing = Array Coordinate

type Polygon =  Polygon (Array LinearRing)


type MultiPoint = MultiPoint (Array Point)

type MultiLineString = MultiLineString (Array LineString)

type MultiPolygon = MultiPolygon (Array Polygon)

type Geometry 
    = Geometry_Point Point
    | Geometry_LineString LineString
    | Geometry_Polygon Polygon
    | Geometry_MultiPoint MultiPoint
    | Geometry_MultiLineString MultiLineString
    | Geometry_MultiPolygon MultiPolygon
