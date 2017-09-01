module VectorLayer exposing (..)


import Html exposing (..)
import Html.Attributes exposing (style)
import Svg exposing (svg, g, path)
import Svg.Attributes exposing ( stroke, d, width, height, viewBox, fill)
import Array
import Array exposing (Array)
import Maybe exposing (Maybe, withDefault)


import Geometry exposing (..)
import Types exposing (..)
import Layer exposing (..)
import Geo exposing (..)

type alias Bounds a =
  { sw : a 
  , ne : a }


type alias GetGeometry = Bounds LatLng -> Cmd (List Geometry)

--type VectorLayerGeometry
  --= VectorLayer_Path Path
  --| VectorLayer_Polygon Polygon

type alias VectorOptions
  =  {  stroke : String
      , color : String
      , weight: String 
      , getGeometry : GetGeometry
    }


type alias VectorLayer -- 
  = { options: VectorOptions
    , geometry :  List Geometry
    , size : Size -- needed (in theory) to calculate when a layer comes  in view
    , latLngOrigin : LatLng
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
        ( { vl | geometry = List.append vl.geometry geos }, Cmd.none )
--- TODO: no! Only do this on dragend
    VectorLayer_Move pos -> 
      let newOrigin = getPannedLatLng vl.crs vl.currentZoom pos vl.latLngOrigin
          bounds = getBounds vl.crs vl.currentZoom vl.size newOrigin
      in 
          ( vl, Cmd.map VectorLayer_Geometry <| vl.options.getGeometry bounds )
    VectorLayer_Zoom zd -> Debug.crash "Basda"

--- VIEWS

  --in 
    --svg
      --[width "500", height "500", viewBox "0 0 500 500"]
      --[node]
 

        --  projectedToPixel crs zoom pnt
    -- difference (product pos tileSize) (projectedToTileName crs zoom origin)
    --tl.crs.projection.project tl.latLngOrigin
vectorLayerView : VectorLayer -> Html VectorLayerAction
vectorLayerView vl =
    let snode =
        svg
          [width "500", height "500", viewBox "0 0 500 500"]
          (List.map (drawGeometry vl.options vl.crs vl.currentZoom vl.size vl.latLngOrigin) vl.geometry)

      
    in
      div
          [ style
            [ ("height", "100%")
            , ("width", "100%")
            , ("position", "absolute")
            , ("z-index", "100")
            ]
          ]
          [snode]

vectorLayersView : List VectorLayer -> Html VectorLayerAction
vectorLayersView vls =
    div
        [ style
          [ ("height", "100%")
          , ("width", "100%")
          ]
        ]
        (List.map vectorLayerView vls)



drawGeometry : VectorOptions -> CRS -> Zoom -> Size -> LatLng -> Geometry -> Svg.Svg msg
drawGeometry vo crs zoom size ll geo =
  case drawPath (getGeometryPath crs zoom size ll geo) False of
    Just pth -> 
      let pnode = path [d pth, stroke vo.stroke, fill "none"] []
      in g [] [pnode]
    Nothing   -> g [] []
      


getCoordinatePosition : CRS -> Zoom -> Size -> LatLng -> Coordinate -> Position
getCoordinatePosition crs zoom size or coord =
    let
        origin = sum {x=0, y=toFloat -size.y} <| latLngToPoint crs zoom or -- sw
        c = latLngToPoint crs zoom {lng=coord.x, lat=coord.y}
    in mapCoord round <| difference c origin

getGeometryPath : CRS -> Zoom -> Size -> LatLng -> Geometry -> Array (Array Position)
getGeometryPath crs zoom size origin geom = 
  let f = getCoordinatePosition crs zoom size origin
      g = Array.map
  in
    Array.map deDupe <|
      case geom of
          Geometry_Point (Point c) -> Array.fromList [ Array.fromList [ f c ] ]
          Geometry_LineString (LineString arr) -> Array.fromList [g f arr]
          Geometry_Polygon (Polygon arr) -> g (\arr2 -> g f arr2 ) arr
          _ -> Debug.crash "ASD"


drawPath : Array (Array Position) -> Bool -> Maybe String
drawPath ls closed = 
  let 
      mkRing lr s = 
        case drawRing lr of
          Just r -> s ++ r
          Nothing -> s

  in 
    case Array.length ls of
      0 -> Nothing
      _ -> 
        let r = Array.foldl mkRing "" ls
        in
          if r == ""
            then Nothing
            else Just <| r ++ (if closed then "z" else "")
        
            

fromJust : Maybe a -> a
fromJust m =
  case m of
      Just v -> v
      Nothing ->  Debug.crash "fromJust"

zip : List a -> List b -> List (a, b)
zip a b =
  case a of
    [] -> []
    (x::xs)  ->  case b of
      [] -> []
      y::ys  -> (x,y)::zip xs ys

zipA : Array a -> Array b -> Array (a, b)
zipA a b =
  let la = Debug.log "FOO" <| Array.length a
      lb = Debug.log "BAR" <| Array.length b 
  in
      case la of
        0 -> Array.empty
        _  ->  case lb of
          0 -> Array.empty
          _  -> 
            let ta = Debug.log "BAZ" <| Array.slice 1 (la + 1) <| Debug.log "A" a
                tb = Debug.log "BOP" <| Array.slice 1 (lb + 1) <| Debug.log "B" b
                ha = fromJust <| Array.get 0 a
                hb = fromJust <| Array.get 0 b
            in Array.append (Array.fromList [(ha, hb)]) <| zipA ta tb

--range : Int -> Int -> Array Int
--range start end = Array.initialize (end - start) (\index -> start + index)


deDupe : Array a -> Array a
deDupe arr = 
  let 
    maybePush el (last, newArr) = 
      if el == last
          then (last, newArr)
          else (el, Array.push el newArr)
    head = fromJust <| Array.get 0 arr
    (_, newArra) = Array.foldl maybePush (head, Array.fromList [head]) arr
  in newArra
      
        

drawRing : Array Position -> Maybe String
drawRing lr  =
  if Array.length lr >= 2
    then
      let 
        mkPoint (i, p) s = 
          let 
            pref = if i == 0 then "M" else "L"
          in 
            s ++ pref ++ (toString p.x) ++ " " ++ (toString p.y)

        lrs = Array.fromList <| zip (List.range 0 (Array.length lr)) <| Array.toList lr

      in 
        Just <| Array.foldl mkPoint "" lrs
    else
      Nothing
