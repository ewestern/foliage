module Pane exposing (..)


import Time exposing (Time, second, now)

import Mouse
import Task
import AnimationFrame

import Json.Decode as Decode
import Html exposing (..)
import Ease exposing (..) 
import Html.Attributes exposing (..)
import Html.Events exposing (on)

--
import Util exposing (px)
import TileLayer exposing (TileLayer, TileLayerAction, viewTileLayer)
import Geo exposing (LatLng, Size, Position)
type alias Displacement = Position -- i.e., where the pane's, left and top coords ought to be
type alias Coords = Position -- i.e., the page coordinates that come from a mouse movement


---
type PaneAction
    = DragStart Coords
    | DragAt Coords
    | DragEnd
    | DragCoastStart Time
    | DragCoast Position
    | DragCoastEnd
    | Pane_Empty

type alias Velocity =
  { dx : Float
  , dy : Float
  }

-- For a given scroll cycle, the Pane's origin can be associated with a specific LatLng
type alias MapPane = 
  { dragstate : DragState
  , tileLayers : List TileLayer
  , vectorLayers : List Int
  , latLngCenter : LatLng
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
  { current : Displacement
  , start : Coords
  , velocity : Velocity -- vector representing deltas (per frame?)
  , release : Maybe Time } 


updateDragState : PaneAction -> DragState -> DragState
updateDragState da ds =
  case ds of
    Just d ->  
      case da of
        DragCoastEnd -> Nothing
        _ -> Just <| updateDrag da d

    Nothing ->  
      case da of
        DragStart xy -> Just <| { defaultDrag | start = xy }
        _ -> Debug.crash "Oops, shouldn't happen"

updateDrag : PaneAction -> Drag -> Drag
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
      Pane_Empty       -> Debug.crash "Shouldn't happen"



onMouseDown : Attribute PaneAction
onMouseDown =
  on "mousedown" (Decode.map DragStart Mouse.position)


coastSubscription : Drag -> Sub PaneAction
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

dragSubscription : DragState -> Sub PaneAction
dragSubscription ds = 
    case ds of
      Just d -> Sub.batch [ Mouse.moves DragAt, Mouse.ups (\p -> DragEnd), coastSubscription d]
      Nothing -> Sub.none
      
---


--
updatePaneCommand : PaneAction -> Cmd PaneAction
updatePaneCommand da = 
    case da of
      DragEnd -> Task.perform DragCoastStart now
      _   -> Cmd.none


-- updateTileLayer : TileLayerAction -> TileLayer -> (TileLayer, Cmd TileLayerAction)
-- case on pane action -> for `move` actions, update TileLayer
-- TileLayer_Move Position


updatePane : PaneAction -> MapPane -> (MapPane, Cmd PaneAction)
updatePane action pane = 
    let cmd = updatePaneCommand action
        dsNew = updateDragState action pane.dragstate
        posNew = Maybe.withDefault pane.position <| Maybe.map (\d -> addVelocity pane.position d.velocity) dsNew
        newPane =  
            { pane |
              dragstate = dsNew
            , position =  posNew }
    in (newPane, cmd)

viewPane : MapPane -> Html PaneAction
viewPane  pane =
      div
        [ onMouseDown
        , style
            [ ("left", px pane.position.x)
            , ("top", px pane.position.y)
            --, ("background-color" , "#3C8D2F")
            , ("height", "100%")
            , ("width", "100%")
            , ("position", "absolute")
            ]
        ] 
        (List.map viewTileLayer pane.tileLayers)


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
  
