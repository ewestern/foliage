module VectorLayer exposing (..)

import Geometry exposing (..)
import Types exposing (..)
import Layer exposing (..)
import Geo exposing (..)

type alias Bounds a =
  { sw : a 
  , ne : a }

type alias LatLng =
   { lng : Float
   , lat : Float }


type alias GetGeometry = Bounds LatLng -> Cmd (List Geometry)

--type VectorLayerGeometry
  --= VectorLayer_Path Path
  --| VectorLayer_Polygon Polygon

type alias VectorOptions
  =  {  stroke : String
      , color : String
      , weight: String 
      , fetchThreshold : Float
      , getGeometry : GetGeometry
    }


type alias VectorLayer -- 
  = { options: VectorOptions
    , geometry :  List Geometry
    , size : Size -- needed (in theory) to calculate when a layer comes  in view
    , latLngOrigin : LatLng
    , lastFetch : Position
    , crs : CRS
    , currentZoom : Zoom }

type VectorLayerAction
   = VectorLayer_Move Position
   | VectorLayer_Zoom ZoomDir
   | VectorLayer_Geometry (List Geometry)

absPixelDiff : Position -> Position -> Float
absPixelDiff x y = 
  let a = abs (x.x - y.x) 
      b = abs (x.y - y.y)
  in sqrt  <| toFloat (a ^ 2 + b ^ 2)

updateVectorLayer : VectorLayerAction -> VectorLayer -> (VectorLayer, Cmd VectorLayerAction)
updateVectorLayer vla vl = 
  case vla of
    VectorLayer_Geometry geos -> 
        ( { vl | geometry = List.append vl.geometry <| Debug.log "geo" geos }, Cmd.none )
--- TODO: no! Only do this on dragend
    VectorLayer_Move pos -> 
      let newOrigin = getPannedLatLng vl.crs vl.currentZoom pos vl.latLngOrigin
          bounds = getBounds vl.crs vl.currentZoom vl.size newOrigin
      in 
        if absPixelDiff vl.lastFetch pos > vl.options.fetchThreshold
          then ( {vl | lastFetch = pos }, Cmd.map VectorLayer_Geometry <| vl.options.getGeometry bounds )
          else (vl, Cmd.none)
    VectorLayer_Zoom zd -> Debug.crash "Basda"

drawPath : List (List Position) -> Bool -> String
drawPath ls closed = 
  let 
      mkRing lr acc = acc ++ drawRing lr
  in 
    case List.length ls of
      0 -> "M0 0"
      _ -> List.foldl mkRing "" ls
            ++ (if closed then "z" else "")


zip : List a -> List b -> List (a, b)
zip a b =
  case a of
    [] -> []
    (x::xs)  ->  case b of
      [] -> []
      y::ys  -> (x,y)::zip xs ys

drawRing : List Position -> String
drawRing lr  =
  let 
     mkPoint (i, p) acc = 
        let pref = if i == 0 then "M" else "L"
        in acc ++ pref ++ (toString p.x) ++ " " ++ (toString p.y)
  in
    List.foldl mkPoint "" <| zip (List.range 0 (List.length lr)) lr
