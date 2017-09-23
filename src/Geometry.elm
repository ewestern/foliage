module Geometry exposing (..)
---import Array exposing (get, foldl)
import List exposing (foldl)
import GeoJson as GJ
import Geo exposing (Point)

type alias Coordinate = GJ.Position -- Let's not get confused with other Positions

first : (a, b, c) -> a
first (a,b,c) = a

second : (a, b, c) -> b
second (a, b, c) = b

type alias Envelope = { min : Point, max: Point }

emptyEnvelope : Envelope
emptyEnvelope = { min = {x=10e10,y=10e10}, max ={x=-10e10,y=-10e10} }

coordinateEnvelope : Coordinate -> Envelope
coordinateEnvelope (x,y,_) = { min={x=x, y=y}, max = {x=x,y=y}}


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
extendEnvelope (x,y,_) env = 
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

geometryToEnvelope : GJ.Geometry -> Envelope
geometryToEnvelope geo =  case geo of
        GJ.LineString ls -> lineStringToEnvelope ls
        _ -> Debug.crash "geometryToEnvelope not yet implemented"

lineStringToEnvelope : List Coordinate -> Envelope
lineStringToEnvelope ls = 
  case ls of
    (c::cs) -> 
      let envInit = coordinateEnvelope c
      in foldl extendEnvelope envInit ls
    []     -> Debug.crash "Cannot create envelope from empty linestring"

