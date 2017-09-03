module Geo exposing (..)

-- A point represents a projected coordinate on a two dimensional plane. It shares an origin with LatLng
type alias Coord number
  = { x : number, y: number }

type alias Size = Coord Int

type alias Point = Coord Float

type alias Position = Coord Int

type alias Zoom = Int

type alias LatLng =
   { lng : Float
   , lat : Float }

mapCoord : (a -> b) -> Coord a -> Coord b
mapCoord f {x,y} =
  { x = f x
  , y = f y }


type alias Bounds a =
  { sw : a 
  , ne : a }

mapBounds : (a -> b) -> Bounds a -> Bounds b
mapBounds f {sw,ne} =
        { sw = f sw
        , ne = f ne }


scaleBy : (number -> number -> number) -> Coord number -> Coord number -> Coord number
scaleBy f p1 p2 =
  { x = f p1.x p2.x
  , y = f p1.y p2.y }

product :  Coord number -> Coord number -> Coord number
product = scaleBy (*)

quotient :  Coord Float -> Coord Float -> Coord Float
quotient = scaleBy (/)

sum :  Coord number -> Coord number -> Coord number
sum = scaleBy (+)

difference :  Coord number -> Coord number -> Coord number
difference = scaleBy (-)

type alias Projection = 
-- Projects geographical coordinates into coordinates in units accepted for
  { project : LatLng -> Point
  , unproject : Point -> LatLng
  , bounds: Bounds Point }

type alias Transformation = (Float, Float, Float, Float)

-- Returns a transformed point, multiplied by the given scale.
transform : Transformation -> Float -> Point -> Point
transform (a, b, c, d) scale point  =  
    {  x = scale * (a * point.x + b)
    ,  y = scale * (c * point.y + d) }

untransform : Transformation -> Float -> Point -> Point
untransform (a, b, c, d) scale point  = 
    { x = (point.x / scale - b) / a
    , y = (point.y / scale - d) / c }


type alias CRS = 
  { code: String
  , projection: Projection
  , scale : Int -> Float
  , transformation: Transformation}


sphericalMercatorProjection : Projection
sphericalMercatorProjection = 
  { project = \{lat,lng} -> 
      let rad = pi / 180
          max_ = 85.0511287798
          lat_ = max (min max_ lat) -max_
          sin_ = sin (lat_ *  rad)
      in 
        { x = earthRadius * lng * rad
        , y = earthRadius * (logBase e ((1 + sin_) / (1 - sin_))) / 2 }
  , unproject = \{x,y} ->  
      let d = 180 / pi
      in { lat = (2 * (atan (e ^ (y / earthRadius))) - (pi / 2) ) * d
         , lng = x * d / earthRadius }

  , bounds = 
      let r = earthRadius
      in {sw = {x = -r, y = -r }, ne = {x = r, y = r} }  
  }
      

earthRadius : Float
earthRadius = 6378137 

espg3857 : CRS
espg3857 =
  { code = "ESPG:3857"
  , projection = sphericalMercatorProjection
  , scale = \zoom -> toFloat <| 256 * (2 ^ zoom)
  , transformation = 
      let scale = 0.5 / (pi * earthRadius)
      in (scale, 0.5, -scale, 0.5) }

projectedBounds : CRS -> Zoom -> Bounds Point
projectedBounds crs zoom = 
  let b = crs.projection.bounds
      s = crs.scale zoom
      min = transform crs.transformation s b.sw
      max = transform crs.transformation s b.ne 
  in 
    { sw = min
    , ne = max }

lonLatProjection : Projection
lonLatProjection = 
  { project = \l -> {x = l.lng, y = l.lat}
  , unproject = \p -> {lng = p.x, lat = p.y}
  , bounds = {sw = {x =-180, y = -90}, ne = {x = 180, y = 90}} }

espg4326 : CRS
espg4326 = 
  { code = "ESPG:4326"
  , projection = lonLatProjection
  , scale = \zoom -> toFloat <| 256 * (2 ^ zoom)
  , transformation = (1 / 180, 1, -1 / 180, 0.5) }


    
type alias TileSpec = 
  { x : Int
  , y : Int
  , z : Int }

distance : LatLng -> LatLng -> Float
distance l1 l2 = 
  let rad = pi / 180
      lat1 = l1.lat  * rad
      lat2 = l2.lat * rad
      a = sin lat1 * sin lat2 +
          cos lat1 * cos lat2 * cos ((l2.lng - l1.lng)  * rad)
  in earthRadius * (acos  (min a 1))

