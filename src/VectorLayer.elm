module VectorLayer exposing (..)


import Html exposing (..)
import Html.Attributes exposing (style, attribute)
import Svg exposing (svg, g, path)
import Svg.Attributes exposing ( stroke, d, width, height, viewBox, fill, x, y)
import Array
import Array exposing (Array)
import Maybe exposing (Maybe, withDefault)

import Geometry exposing (..)
import Layer exposing (..)
import Geo exposing (..)
import GeoJson as GJ

type alias GetGeometry = Bounds LatLng -> Cmd (List GJ.Geometry)


type alias VectorOptions
  =  {  stroke : String
      , color : String
      , weight: String 
      , getGeometry : GetGeometry
    }


type alias VectorLayer -- 
  = { options: VectorOptions
    , geometry :  List GJ.Geometry
    , size : Size -- needed (in theory) to calculate when a layer comes  in view
    , latLngOrigin : LatLng
    , crs : CRS
    , currentZoom : Zoom }

type VectorLayerAction
   = VectorLayer_Move Position
   | VectorLayer_Zoom ZoomDir
   | VectorLayer_ZoomInOn Position
   | VectorLayer_Geometry (List GJ.Geometry)

batchUpdateVector : VectorLayerAction -> List VectorLayer -> (List VectorLayer, Cmd VectorLayerAction)
batchUpdateVector vla vls = 
  let newVLS = List.map(updateVectorLayer vla) vls
      (vvls, vcmd) = List.foldr (\(vl, c) (vs, cs) -> (vl::vs ,c::cs)) ([], []) newVLS
  in (vvls, Cmd.batch vcmd)

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
    VectorLayer_Zoom zd -> 
      let nz = incZoom zd vl.currentZoom
          center = getCenterFromOrigin vl.crs vl.currentZoom vl.size vl.latLngOrigin
          newOrigin = getOriginFromCenter vl.crs nz vl.size center 
          bounds = getBounds vl.crs vl.currentZoom vl.size newOrigin
          nvl = 
            { vl | 
              currentZoom = nz
            , latLngOrigin = newOrigin }
      in (nvl, Cmd.map VectorLayer_Geometry <| vl.options.getGeometry bounds )
    VectorLayer_ZoomInOn pos -> 
      let nz = vl.currentZoom + 1
          center = getCenterFromOrigin vl.crs vl.currentZoom vl.size vl.latLngOrigin
          targetCenter = getPannedLatLng vl.crs vl.currentZoom pos center
          newOrigin = getOriginFromCenter vl.crs nz vl.size targetCenter 
          bounds = getBounds vl.crs vl.currentZoom vl.size newOrigin
          nvl = 
            { vl | 
              currentZoom = nz
            , latLngOrigin = newOrigin }
      in (nvl, Cmd.map VectorLayer_Geometry <| vl.options.getGeometry bounds )




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

type alias ViewBox = (Int, Int, Int, Int)
type alias Translation = (Int, Int, Int)
type alias Dimensions = (Int, Int)

getBoxFromBounds : CRS -> Zoom -> Size -> LatLng -> Bounds LatLng -> (Dimensions, ViewBox, Translation)
getBoxFromBounds crs zoom size ll bds = 
  let pBounds = mapBounds (latLngToPoint crs zoom) bds
      or = latLngToPoint crs zoom ll
      t =  round <| pBounds.ne.y - or.y - (toFloat size.y)
      l = round <| or.x - pBounds.sw.x
      w = (abs <| round <| pBounds.ne.x - or.x)  + size.x
      h = max (size.y - t) size.y
      box = (l, t, w, h)
      trans = (l, t, 0)
  in ((w, h), box, trans)
-- min-x, min-y, width and height


--vectorLayerSVG : VectorLayer
getSVGAttributes : VectorLayer -> List (Svg.Attribute a)
getSVGAttributes vl = 
    let vectorBounds = Maybe.map envelopeToBounds <| getVectorLayerEnvelope vl
    in
      case vectorBounds of
        Just bs -> 
          let ((w, h), (l, t, vw, vh), (tl, tt, z)) = getBoxFromBounds vl.crs vl.currentZoom vl.size vl.latLngOrigin bs
              box = String.join " " <| List.map toString [l, t, vw, vh]
              trans = String.join "," <| List.map (flip (++) "px") <| List.map toString [tl, tt, 0]
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



drawGeometry : VectorOptions -> CRS -> Zoom -> Size -> LatLng -> GJ.Geometry -> Svg.Svg msg
drawGeometry vo crs zoom size ll geo =
  case drawPath (getGeometryPath crs zoom size ll geo) False of
    Just pth -> 
      let pnode = path [d pth, stroke vo.stroke, fill "none"] []
      in g [] [pnode]
    Nothing   -> g [] []
      


getCoordinatePosition : CRS -> Zoom -> Size -> LatLng -> Coordinate -> Position
getCoordinatePosition crs zoom size or (x,y,_) =
  let
    origin = sum {x=0, y=toFloat -size.y} <| latLngToPoint crs zoom or -- sw
    c = latLngToPoint crs zoom {lng=x, lat=y}
  in mapCoord round <| difference c origin

getGeometryPath : CRS -> Zoom -> Size -> LatLng -> GJ.Geometry -> List (List Position)
getGeometryPath crs zoom size origin geom = 
  let f = getCoordinatePosition crs zoom size origin
  in
    List.map deDupe <|
      case geom of
          GJ.Point c -> [ [ f c ] ]
          GJ.LineString ls -> [List.map f ls]
          GJ.Polygon lss -> List.map (\arr2 -> List.map f arr2 ) lss
          _ -> Debug.crash "ASD"


drawPath : List (List Position) -> Bool -> Maybe String
drawPath ls closed = 
  let 
      mkRing lr s = 
        case drawRing lr of
          Just r -> s ++ r
          Nothing -> s

  in 
    case ls of
      [] -> Nothing
      _ -> 
        let r = List.foldl mkRing "" ls
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

deDupeF : a -> (a, List a) -> (a, List a)
deDupeF item (last,ls) = 
  if item == last
    then (last, ls)
    else (item, List.append ls [item])
  


deDupe : List a -> List a
deDupe ls = 
  case ls of
    (x::xs) -> 
      let init = (x, [x])
          (final, ls) = List.foldl deDupeF init xs
      in ls
    []  -> []

drawRing : List Position -> Maybe String
drawRing lr  =
  if List.length lr >= 2
    then
      let 
        mkPoint (i, p) s = 
          let 
            pref = if i == 0 then "M" else "L"
          in 
            s ++ pref ++ (toString p.x) ++ " " ++ (toString p.y)

        lrs = zip (List.range 0 (List.length lr)) lr

      in 
        Just <| List.foldl mkPoint "" lrs
    else
      Nothing
