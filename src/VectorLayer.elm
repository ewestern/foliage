module VectorLayer exposing (..)


import Html exposing (..)
import Html.Attributes exposing (style, attribute)
import Svg exposing (svg, g, path)
import Svg.Attributes exposing ( stroke, d, width, height, viewBox, fill, x, y)
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

updateVectorLayer : VectorLayerAction -> VectorLayer -> (VectorLayer, Cmd VectorLayerAction)
updateVectorLayer vla vl = 
  case vla of
    VectorLayer_Geometry geos -> 
-- For now, just abandon previous retrieved geometries. Probably want to do some kind of caching
        ( { vl | geometry = geos }, Cmd.none )
    VectorLayer_Move pos -> 
      let disp = mapCoord negate pos
          newOrigin = getPannedLatLng vl.crs vl.currentZoom disp vl.latLngOrigin
          bounds = getBounds vl.crs vl.currentZoom vl.size newOrigin
      in 
          ( vl, Cmd.map VectorLayer_Geometry <| vl.options.getGeometry bounds )
    VectorLayer_Zoom zd -> Debug.crash "Basda"

--- VIEWS

envelopeToBounds : Envelope -> Bounds LatLng
envelopeToBounds {min,max} = {sw={lat=min.y, lng=min.x},ne={lat=max.y, lng=max.x}}

getVectorLayerEnvelope : VectorLayer -> Maybe Envelope 
getVectorLayerEnvelope vl = case vl.geometry of
        []  -> Nothing
        [x] -> Just <| geometryToEnvelope x
        (x::xs) -> 
            let f geo env = unionEnvelope env <| geometryToEnvelope geo
            in Just <| List.foldl f (geometryToEnvelope x)  xs


--vectorLayerSVG : VectorLayer
getSVGAttributes : VectorLayer -> List (Svg.Attribute a)
getSVGAttributes vl = 
    let vectorBounds = Maybe.map envelopeToBounds <| getVectorLayerEnvelope vl
    in
      case vectorBounds of
        Just bs -> 
          let pBounds = mapBounds (latLngToPoint vl.crs vl.currentZoom) bs
              or = latLngToPoint vl.crs vl.currentZoom vl.latLngOrigin
              t = round <| pBounds.ne.y - or.y - (toFloat vl.size.y)
              l = round <| or.x - pBounds.sw.x
              w = vl.size.x - l
              h = vl.size.y - t
              box = String.join " " <| List.map toString [l, t, w, h]
              trans = String.join "," <| List.map (\i -> i ++ "px") <| List.map toString [l, t, 0]
              translate = ("transform", "translate3d(" ++ trans ++ ")") 
          in [width <| toString w, height <| toString h, viewBox box, style [translate]]
        Nothing -> [width "100%", height "100%"]
           

vectorLayerView : VectorLayer -> Html VectorLayerAction
vectorLayerView vl =
    let snode =
          svg
            (getSVGAttributes vl )
            (List.map (drawGeometry vl.options vl.crs vl.currentZoom vl.size vl.latLngOrigin) vl.geometry)

    in
      div
          [ style
            [ ("height", "100%")
            , ("width", "100%")
            , ("position", "absolute")
            , ("z-index", "100")
            ]
          , attribute "data-foliage-name" "vector-layer-view"
          ]
          [snode]

vectorLayersView : List VectorLayer -> Html VectorLayerAction
vectorLayersView vls =
    div
        [ style
          [ ("height", "100%")
          , ("width", "100%")
          , ("position", "absolute")
          ]
          , attribute "data-foliage-name" "vector-layers-view"
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
