module Map exposing (..)
import Mouse exposing (Position)
import Time exposing (Time)

type alias Velocity =
  { dx : Float
  , dy : Float
  }

-- the map window
type alias Map = 
  { size : Position 
  , pane: MapPane
  }

type alias Layer =
  { foo : Int
  }

-- the thing you drag
type alias MapPane = 
  { position : Position
  , drag : Maybe Drag
  , layers: List Layer
  }

type alias Drag =
    { start : Position
    , current : Position
    , velocity : Velocity -- vector representing deltas (per frame?)
    , release : Maybe Time -- time of release
    }
