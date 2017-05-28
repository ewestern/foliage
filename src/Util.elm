module Util exposing (..)

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




