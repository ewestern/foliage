module Pane exposing (..)


import Time exposing (Time, second, now)

import Mouse
import Task
import AnimationFrame
import Svg exposing (rect, svg)
import Svg.Attributes  as SA

import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onDoubleClick)

--
import Util exposing (px)
import Layer exposing (ZoomDir(..))
import TileLayer exposing (..)
import VectorLayer exposing (VectorLayer, VectorOptions, VectorLayerAction(..), updateVectorLayer, vectorLayersView, batchUpdateVector)
import Geo exposing (LatLng, Size, Position)


---

type DragAction
    = DragStart Position
    | DragAt Position
    | DragEnd
    | DragCoastStart Time
    | DragCoast Position
    | DragCoastEnd

type PaneAction
    = Pane_Drag DragAction
    | Pane_Zoom ZoomDir
    | Pane_ZoomInOn Position
    | Pane_Vector VectorLayerAction 
    | Pane_Tile TileLayerAction
    | Pane_Empty


type alias Velocity =
  { dx : Float
  , dy : Float
  }

type alias MapPane = 
  { dragstate : DragState
  , tileLayers : List TileLayer
  , vectorLayers : List VectorLayer
  , size : Size
  , position : Position
  }

scale : Velocity -> Float -> Velocity
scale {dx,dy} i = {dx = dx * i, dy = dy * i }

scaleP : Position -> Float -> Velocity
scaleP {x,y} i = { dx = (toFloat x) * i, dy = (toFloat y) * i }

addVelocity : Position -> Velocity -> Position
addVelocity {x,y} {dx,dy} = { x = (round dx) + x, y = (round dy) + y }

addPosition : Position -> Position -> Position
addPosition {x,y} p2 = {x = x + p2.x, y = p2.y + y }

sub : Position -> Position -> Position
sub p1 p2 = {x = p1.x - p2.x, y = p1.y - p2.y }

type alias DragState = Maybe Drag

type alias Drag =
  { current : Position
  , start : Position
  , velocity : Velocity -- vector representing deltas (per frame?)
  , release : Maybe Time } 


updateDragState : DragAction -> DragState -> DragState
updateDragState da ds =
  case ds of
    Just d ->  
      case da of
        DragCoastEnd -> Nothing
        _ -> Just <| updateDrag da d

    Nothing ->  
      case da of
        DragStart xy -> Just <| { defaultDrag | start = xy }
        -- TODO: handle double clicks!!
        _ -> Debug.crash "Oops, shouldn't happen"

updateDrag : DragAction -> Drag -> Drag
updateDrag action state  = 
    case action of
      DragStart xy     ->  { state | start = xy }
      DragAt xy        -> 
        case state.release of
          Just _ -> state
          Nothing -> 
            let np = sub xy state.start
                v = scaleP (sub np state.current) 1.0
            in { state | current =  np, velocity = v }
      DragEnd           ->  state
      DragCoastStart t  -> { state | release = Just t }
      DragCoast np      -> { state | current = np } 
      DragCoastEnd      ->  Debug.crash "Shouldn't happen"



onMouseDown : (Position -> a) -> Attribute a
onMouseDown f =
  on "mousedown" (Decode.map f Mouse.position)


coastSubscription : Drag -> Sub DragAction
coastSubscription state = 
    case state.release of
      Nothing -> Sub.none
      Just t0 -> 
        let f t = 
            let fact = easer ((t - t0)  / 100)
                v = scale state.velocity fact
                next = addVelocity state.current  v 
            in 
              if almostZero v 
                then DragCoastEnd
                else DragCoast next
        in Sub.map f <| AnimationFrame.times identity

easer : Float -> Float
easer exp = 
  1 / (1 + e ^ (10 * (exp - 0.5)))

epsilon = 0.001

almostZero : Velocity -> Bool
almostZero {dx,dy} = (abs dx < epsilon) && (abs dy < epsilon)

dragSubscription : DragState -> Sub DragAction
dragSubscription ds = 
    case ds of
      Just d -> Sub.batch [ Mouse.moves DragAt, Mouse.ups (\p -> DragEnd), coastSubscription d]
      Nothing -> Sub.none
      
updateDragCommand : DragAction -> Cmd DragAction
updateDragCommand da = 
    case da of
      DragEnd -> Task.perform DragCoastStart now
      _   -> Cmd.none


updateVectorLayerWithDragAction : DragAction -> Position -> List VectorLayer -> (Cmd PaneAction, List VectorLayer)
updateVectorLayerWithDragAction da pos vls = 
  case da of
    DragCoastEnd  -> 
        let rs = List.map(updateVectorLayer <| VectorLayer_Move pos) vls
            (newvls, vcmd) = List.foldr (\(vl, c) (vs, cs) -> (vl::vs ,(Cmd.map Pane_Vector c)::cs)) ([], []) rs
        in (Cmd.batch vcmd, newvls)
    _             -> (Cmd.none, vls)


updatePane : PaneAction -> MapPane -> (MapPane, Cmd PaneAction)
updatePane action pane = 
    case action of
      Pane_Drag da -> 
        let dsNew = updateDragState da pane.dragstate
            posNew = Maybe.withDefault pane.position <| Maybe.map (\d -> addVelocity pane.position d.velocity) dsNew
            (vcmd, vls) = updateVectorLayerWithDragAction da posNew pane.vectorLayers
            newPane = 
                { pane |
                  dragstate = dsNew
                , tileLayers = List.map (updateTileLayer <| TileLayer_Move posNew ) pane.tileLayers
                , vectorLayers = vls
                , position =  posNew }
            dragCmd = Cmd.map Pane_Drag <| updateDragCommand da
            cmd = Cmd.batch [dragCmd, vcmd]
        in (newPane, cmd)
      Pane_Zoom zd ->  
-- TODO: update vector layer
-- TODO: might need to reset pane.position since we're resetting latLngOrigin ??
        let (vls, vcmd) = batchUpdateVector (VectorLayer_Zoom zd) pane.vectorLayers
            np =  
            { pane | 
                tileLayers = List.map (updateTileLayer (TileLayer_Zoom zd)) pane.tileLayers  
              , vectorLayers = vls
            } 
        in (np, Cmd.map Pane_Vector vcmd)
      Pane_Vector vla -> 
        let (vls, vcmd) = batchUpdateVector vla pane.vectorLayers
        in ({ pane | vectorLayers = vls }, Cmd.map Pane_Vector vcmd) 
      Pane_Tile tla -> 
        let newtls = List.map (updateTileLayer tla) pane.tileLayers
        in ( { pane | tileLayers = newtls }, Cmd.none)
         
      Pane_Empty -> Debug.crash "FOO"
          
      
viewPane : MapPane -> Html PaneAction
viewPane  pane =
      div
        [ onMouseDown (Pane_Drag << DragStart)
        , onDoubleClick (Pane_Zoom In) 
        , attribute "data-foliage-name" "view-pane"
        , style
            [ 
              ("height", "100%")
            , ("width", "100%")
            , ("position", "absolute")
            ]
        ] 
        [ viewContainer pane.position pane.tileLayers pane.vectorLayers
        , Html.map Pane_Zoom <| zoomContainer {x=10, y=10} 
        ]
-- idea is: Pane stays still; child Container moves


zoomContainer : Position -> Html ZoomDir
zoomContainer pos =
    div
      [ style
        [ ("border", "2px solid rgba(0,0,0,0.2)")
        , ("margin-left", px pos.x)
        , ("margin-top", px pos.y)
        , ("float", "left")
        , ("clear", "both")
        , ("z-index", "1000")
        , ("position", "relative")
        , ("cursor", "auto")
        ]
      ]
    [ zoomButtonView In, zoomButtonView Out ]
    

zoomButtonView : ZoomDir -> Html ZoomDir
zoomButtonView s = 
      a
        [ onMouseDown <| always s
        , attribute "role" "button"
        , attribute "href" "#"
        , style
            [ ("height", "30px")
            , ("width", "30px")
            , ("font-size", "22px")
            , ("text-align", "center")
            , ("display", "block")
            , ("background-color", "#fff")
            , ("text-decoration", "none")
            ]
        ]
        [zoomIconView s]

zoomIconView : ZoomDir -> Html a
zoomIconView z = case z of
    In -> text "+"
    Out -> text "-"
    

icon : Html a
icon = 
  let r = rect [ SA.width "40", SA.height "40"] []
  in svg [] [r]

viewContainer : Position -> List TileLayer -> List VectorLayer -> Html PaneAction
viewContainer pos ls vls =
    let tls = (List.map (Html.map Pane_Tile << viewTileLayer) ls)
        vl = Html.map Pane_Vector <| vectorLayersView vls
    in
        div 
          [ style
              [ ( "left", px pos.x)
              , ( "top", px pos.y)
              , ("height", "100%")
              , ("width", "100%")
              , ("position", "absolute")
              , ("cursor",  "-webkit-grab")
              , ("pointer-events", "none")
            ]

          , attribute "data-foliage-name" "view-container"
          ]
          (vl::tls)

emptyPos = {x = 0, y = 0}
emptyVel = {dx = 0.0, dy = 0.0 }

defaultDrag : Drag
defaultDrag = 
  { current = emptyPos
  , start = emptyPos
  , velocity = emptyVel
  , release = Nothing }


--- Generalize to All Layers

addTileLayer : MapPane -> TileLayer -> MapPane
addTileLayer mp tl = { mp | tileLayers = tl :: mp.tileLayers }
  

