module Layer exposing (..)

import Geo exposing (..)

type ZoomDir
  = In
  | Out

type alias Layer a 
  = { a | 
      size : Size
    --, latLngOrigin : LatLng
    , crs : CRS
    , currentZoom : Zoom }

incZoom : ZoomDir -> Zoom -> Zoom
incZoom zd z =  case zd of
    In -> z + 1
    Out -> z - 1


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

getCenterFromOrigin : CRS -> Zoom -> Size -> LatLng -> LatLng
getCenterFromOrigin crs zoom size ll = 
    let point = latLngToPoint crs zoom ll
        half = mapCoord (\i -> (toFloat i /2)) size
        centerPoint =  { y = point.y - half.y, x = point.x + half.x }
    in pointToLatLng crs zoom centerPoint

getOriginFromCenter : CRS -> Zoom -> Size -> LatLng -> LatLng
getOriginFromCenter crs zoom size ll = 
    let half = mapCoord ((*) 0.5 << toFloat) size
        or = latLngToPoint crs zoom ll
        swp = { y=or.y + half.y , x=or.x - half.x }
    in pointToLatLng crs zoom swp


