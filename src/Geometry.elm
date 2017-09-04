module Geometry exposing (..)
import Types exposing(..)
import Array exposing (get, foldl)



type alias Envelope = { min : Coordinate, max: Coordinate }

emptyEnvelope : Envelope
emptyEnvelope = { min = {x=10e10,y=10e10}, max ={x=-10e10,y=-10e10} }

coordinateEnvelope : Coordinate -> Envelope
coordinateEnvelope p = { min=p, max = p}


extendEnvelopeX x env = 
  if x < env.min.x
    then { env | min = {y=env.min.y, x = x } }
    else 
      if x > env.max.x
        then { env | max = { y=env.max.y, x = x } }
        else env

extendEnvelopeY y env = 
  if y < env.min.y
    then { env | min = { x =env.min.x, y = y } }
    else 
      if y > env.max.y
        then { env | max = { x = env.max.x, y = y } }
        else env


extendEnvelope : Coordinate -> Envelope -> Envelope
extendEnvelope {x,y} env = 
  let envY = extendEnvelopeY y env
      envX  = extendEnvelopeX x env
  in {  min =
        { x=envX.min.x
        , y= envY.min.y }
      , max = 
        { x = envX.max.x
        , y = envY.max.y } 
      } 

unionEnvelope : Envelope -> Envelope -> Envelope
unionEnvelope env1 env2 = 
        let n = { x = min env1.min.x env2.min.x, y = min env1.min.y env2.min.y}
            x = { x = max env1.max.x env2.max.x, y = max env1.max.y env2.max.y }
        in { min = n, max = x }

geometryToEnvelope : Geometry -> Envelope
geometryToEnvelope geo =  case geo of
        Geometry_LineString ls -> lineStringToEnvelope ls
        _ -> Debug.crash "geometryToEnvelope not yet implemented"

lineStringToEnvelope : LineString -> Envelope
lineStringToEnvelope (LineString ls) = 
  case get 0 ls of
    Just c -> 
      let envInit = coordinateEnvelope c
      in foldl extendEnvelope envInit ls
    Nothing -> Debug.crash "Cannot create envelope from empty linestring"

