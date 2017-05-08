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



