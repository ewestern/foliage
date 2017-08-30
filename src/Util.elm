module Util exposing (..)
import Dict
import GeoJSON
import Dict
import Types exposing (..)
import Json.Decode as D
import String
import Result

px : Int -> String
px number =
  toString number ++ "px"

catMaybe : List (Maybe a) -> List a
catMaybe ls =
  case ls of
    (x::xs) -> 
      case x of
        Just a -> a::catMaybe xs
        Nothing -> catMaybe xs
    [] -> []

range : Int -> Int -> List Int
range start end = 
  let 
    rangeF current ls = 
      if current < start 
        then ls 
        else rangeF (current - 1) (current :: ls)
  in
    rangeF end []

zip : List a -> List b -> List (a, b)
zip a b = case a of
    [] -> []
    x::xs ->  case b of
        [] -> []
        y::ys ->  (x,y)::zip xs ys

type alias Segment = 
  { trailType : String
  , sacScale : Maybe String
  , geometry : Geometry
  }

type alias Ref = Int

 

decodeRef : D.Decoder Ref
decodeRef = 
  let f str = Result.withDefault 0 <| String.toInt str
  in D.map f D.string 

decodeSegment : D.Decoder Segment
decodeSegment = 
    D.map3 Segment 
      (D.field "trailType" D.string)
      (D.field "sacScale" <| D.nullable D.string)
      (D.field "geometry" GeoJSON.decodeGeometry )

segmentPairDecoder : D.Decoder (Ref, Segment)
segmentPairDecoder = 
  let ds = D.index 1 decodeSegment
      dr = D.index 0 decodeRef
  in D.andThen (\seg -> D.map (\r -> (r, seg)) dr) ds

segmentDictDecoder : D.Decoder (Dict.Dict Ref Segment)
segmentDictDecoder = D.map Dict.fromList <| D.list segmentPairDecoder


