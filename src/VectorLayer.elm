module VectorLayer exposing (..)

import Geometry exposing (..)


type alias VectorLayerOptions
  = { stroke: String
    , color : String
    , weight: String }

type alias VectorLayer
  = { geometry : Bounds LatLng -> Cmd (List Geometry)
    , options: VectorLayerOptions
    }

--type VectorLayerGeometry
  --= 


--type VectorLayer = VectorLayer VectorLayerOptions 

