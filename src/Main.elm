module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Map exposing (..)
import Mouse exposing (..)
import Html.Events exposing (on)
import Json.Decode as Decode
import AnimationFrame
import Ease exposing (outExpo, inExpo) -- Float -> Float
import Debug
import Time exposing (..)
import Task


-- MODEL

--type alias Map = { }


---- UPDATE

type DragAction
    = DragStart Position
    | DragAt Position
    | DragEnd Position
    | DragCoastStart Time
    | DragCoast Velocity
    | DragCoastEnd Position

type Action
    = PaneAction PaneAction

type PaneAction
    = DragAction DragAction





defaultMap = 
  { size = { x = 300, y =  300 },
    pane =  { position = { x = 0, y = 0 }, drag = Nothing, layers = [] }
  }

init : (Map, Cmd Action)
init = (defaultMap, Cmd.none)

px : Int -> String
px number =
  toString number ++ "px"

view : Map -> Html Action
view mp =  Html.map PaneAction (paneView mp.pane)

paneView : MapPane -> Html PaneAction
paneView pane = 
  let pos = getPosition pane
  --let pos = pane.position
---  let translate = "translate3d(" ++ (String.join ", " [(toString p.position.x) ++ "px", (toString p.position.y) ++ "px", "0" ] ) ++ ")"
  in
      div
        [ Html.Attributes.map DragAction onMouseDown
        , style
            [ ("background-color" , "#3C8D2F")
            , ("left", px pos.x)
            , ("top", px pos.y)
            , ("height", px 500)
            , ("width", px 500)
            , ("position", "absolute")

            ]
        ] 
        [ text "Foo"]



update : Action -> Map -> ( Map, Cmd Action )
update action ({pane} as mp) =
  case action of
    PaneAction pa -> 
      let (np, cm) = updatePane pa pane
      in ({mp | pane = np }, Cmd.map PaneAction cm)

sub : Position -> Position -> Velocity
sub p1 p2 = {dx = toFloat <| p1.x - p2.x, dy = toFloat <| p1.y - p2.y }

updatePane : PaneAction -> MapPane -> (MapPane, Cmd PaneAction)
updatePane action ({position,drag,layers} as pane) = 
  case action of
    DragAction da -> 
      case da of
        DragCoastEnd st -> 
-- drag cannot become nothing here.
          let np = { pane | position = getPosition pane, drag = Nothing }
          in (np, Cmd.none)
        _         -> 
          let (md, dcmd) = updateDrag da drag
          in ( { pane | drag = md, position = getPosition pane }, Cmd.map DragAction <| dcmd)

-- TODO: this is the weird part. Not sure why it's changing so quick.
getPosition : MapPane -> Position
getPosition {position, drag} =
  case drag of
    Nothing ->
      position
    Just {start,current, velocity} ->
      Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)




updateDrag : DragAction -> Maybe Drag -> (Maybe Drag, Cmd DragAction)
updateDrag action od = 
  case action of
    DragStart xy  ->  
      let drag = (Drag xy xy (Velocity 0 0) Nothing )
      in (Just drag, Cmd.none)
    DragAt xy     ->  
      let f d = case d.release of
        Just _ ->  d
        Nothing -> { d | current = xy, velocity = scale (sub d.current xy) 0.001 }
      in (Maybe.map f od, Cmd.none)
    DragEnd xy   -> 
      let d = Maybe.map (\{start,current,velocity,release} -> Drag start current (scale velocity 0.001) release) od
      in (d, Task.perform DragCoastStart now)
    DragCoastStart time -> 
      let drag = Maybe.map (\{start,current,velocity, release} -> Drag start current (scale velocity 0.5) (Just time) ) od
      in (drag, Cmd.none)
    DragCoast vel ->
      let drag = Maybe.map (\{start,current,velocity, release} -> Drag start (add vel current) vel release ) od
      in  (drag, Cmd.none)
    DragCoastEnd _ -> Debug.crash "Shouldn't happen"


scale : Velocity -> Float -> Velocity
scale {dx,dy} i = {dx = dx * i, dy = dy * i }

add : Velocity -> Position -> Position
add {dx,dy} {x,y} = { x = (round dx) + x, y = (round dy) + y }



onMouseDown : Attribute DragAction
onMouseDown =
  on "mousedown" (Decode.map DragStart Mouse.position)

dragSubscriptions : Maybe Drag -> Sub DragAction
dragSubscriptions md =
  case md of
    Nothing ->
      Sub.none
    Just drag ->
-- TODO
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd, coastSubscription drag]

-- so, the issue is that we need an incrementing time, or some derivative, to be maintained  --- i.e., an argument to DragCoast

coastSubscription : Drag -> Sub DragAction
coastSubscription {start, current, velocity, release} = 
--- can get 
  case release of
    Nothing -> Sub.none
    Just t0 -> 
-- the more time has elapsed, the bigger the denominated and the smaller the velocity
      let help t1 v = 0.00001 * v * inExpo ((inMilliseconds t1)  - (inMilliseconds t0))
          f t1 = 
            if noVelocity velocity
              then DragCoastEnd current
              else DragCoast (Velocity (help t1 velocity.dx) (help t1 velocity.dy))
      in Sub.map f <| AnimationFrame.times identity

noVelocity : Velocity -> Bool
noVelocity {dx,dy} =
  let adx = abs dx
      ady = abs dy
      threshold = 0.1
  in adx < threshold && ady < threshold
  


mapSubscriptions : Map -> Sub Action
mapSubscriptions mp = Sub.map (PaneAction << DragAction) <| dragSubscriptions mp.pane.drag

-- when drag is release, we perform an action that fetches the current time.  This results in a return action of the given time, when that return action comes, we set the model. 

-- if we are in a certain state, we want to subscribe to Animation.times
--- s.t. the model is updated for a certain time

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = mapSubscriptions
    }
