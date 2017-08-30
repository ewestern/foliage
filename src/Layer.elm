module Layer exposing (..)
{-
Does TileLayer, VectorLayer contain Layer
Or Vice versa?


-}
import Geo exposing (..)

type ZoomDir
  = In
  | Out

type alias Layer a 
  = { a | 
      size : Size
    , latLngOrigin : LatLng
    , crs : CRS
    , currentZoom : Zoom }


-- transform the point such that, instead of represention a point on earth, it represents a "point" on a grid (2^zoom) x (2^zoom) in size
projectedToPixel : CRS -> Zoom -> Point -> Point 
projectedToPixel crs zoom point =
  let scalar = crs.scale zoom
  in transform crs.transformation scalar point

pixelToProjected : CRS -> Zoom -> Point -> Point
pixelToProjected crs zoom coord = 
  let scalar = crs.scale zoom
  in  untransform crs.transformation scalar coord


latLngToPoint : CRS -> Zoom -> LatLng -> Point
latLngToPoint crs zoom ll = 
  let scalar = crs.scale zoom
      pp = crs.projection.project ll
  in transform crs.transformation scalar pp

pointToLatLng : CRS -> Zoom -> Point -> LatLng
pointToLatLng crs zoom pos = 
  let scalar = crs.scale zoom
      pp = untransform crs.transformation scalar pos
  in crs.projection.unproject pp

getPannedLatLng : CRS -> Zoom -> Position -> LatLng -> LatLng
getPannedLatLng crs zoom pos ll = 
  -- project latlng on to point on flattened surface of earth
  let point = latLngToPoint crs zoom ll
      newPoint = sum point <| mapCoord toFloat { pos | y = -pos.y }
  in pointToLatLng crs zoom newPoint

-- Assume we are deriving from sw of bounds
getBounds : CRS -> Zoom -> Size -> LatLng -> Bounds LatLng
getBounds crs zoom size ll = 
  let ne = getPannedLatLng crs zoom size ll
  in {sw=ll, ne= ne}



