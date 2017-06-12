module Geometry exposing (..)
import Types exposing(..)
import Array exposing (get, foldl)



type alias Envelope = { min : Coordinate, max: Coordinate }

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

lineStringToEnvelope : LineString -> Envelope
lineStringToEnvelope (LineString ls) = 
  case get 0 ls of
    Just c -> 
      let envInit = coordinateEnvelope c
      in foldl extendEnvelope envInit ls
    Nothing -> Debug.crash "Cannot create envelope from empty linestring"

