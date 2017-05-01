
module Map exposing (..)

import Pane exposing (..)
import Mouse exposing (Position)
import Html

type alias Size = Position

type alias Map
  = { size : Size
    ,  pane : MapPane }

type Action
    = A DragAction

defaultMap = 
  { size = { x = 100, y =  100 },
    pane =  defaultPane
  }

-- paneView : MapPane -> Html PaneAction
mapView : Map -> Html.Html Action
mapView  map =  Html.map A <| viewPane map.pane


--updatePane : DragAction -> MapPane -> (MapPane, Cmd DragAction)
updateMap : Action -> Map -> (Map, Cmd Action)
updateMap action map = 
  case action of
    A da -> 
      let (mp, cd) = updatePane da map.pane
          nm = { map | pane = mp }
          cmd  = Cmd.map A cd
      in (nm, cmd)

--dragSubscription : DragState -> Sub DragAction

mapSubscriptions  : Map -> Sub Action
mapSubscriptions map = 
     Sub.map A <| dragSubscription map.pane.dragstate 
