module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Pane exposing (..)
import Geo exposing (..)
import TileLayer exposing (..)

--type alias DragState =
  --{ position: Displacement
  --, start : Coords
  ----, current : Position
  --, velocity : Velocity -- vector representing deltas (per frame?)
  --, release : Maybe Time } 

emptyPos = {x = 0, y = 0 }

initialDrag = 
  { current = emptyPos
  , start = emptyPos
  , velocity = {dx = 0.0, dy = 0.0 }
  , release = Nothing }

all : Test
all =
    describe "Sample Test Suite"
        [ describe "Foliage test"
            [ test "updateDragState" <|
                \() -> 
                    let 
                      start = { x = 32, y = 23 }
                      ds1 = updateDragState (DragStart start) (Just initialDrag)
                    in  
                        Expect.equal ds1 (Just { initialDrag | start = start })
            ]
        , describe "Geo tests"
-- given a LatLng
            [ 
              test "getTiledPixelBounds" <|
                \() ->
                    let latlng = {lat=36.5, lng= -118.0}
                        bounds = getTiledPixelBounds espg4326 10 {x=256, y=256} latlng
                        actual = { sw = { x = 90294.0439561632, y = 77915.02173394097 }, ne = { x = 90294.0449327257, y = 77915.02271050347 } }
                    in Expect.equal bounds actual
            , test "pixelBoundsToTileRange" <|
                \() ->
                    let size = { x = 256 , y = 256 }
                    in Expect.equal (3 + 7) 10
            , test "String.left" <|
                \() ->
                    Expect.equal "a" (String.left 1 "abcdefg")
            ]
        , describe "Unit test examples"
            [ test "Addition" <|
                \() ->
                    Expect.equal (3 + 7) 10
            , test "String.left" <|
                \() ->
                    Expect.equal "a" (String.left 1 "abcdefg")
            ]
        , describe "Fuzz test examples, using randomly generated input"
            [ fuzz (list int) "Lists always have positive length" <|
                \aList ->
                    List.length aList |> Expect.atLeast 0
            , fuzz (list int) "Sorting a list does not change its length" <|
                \aList ->
                    List.sort aList |> List.length |> Expect.equal (List.length aList)
            , fuzzWith { runs = 1000 } int "List.member will find an integer in a list containing it" <|
                \i ->
                    List.member i [ i ] |> Expect.true "If you see this, List.member returned False!"
            , fuzz2 string string "The length of a string equals the sum of its substrings' lengths" <|
                \s1 s2 ->
                    s1 ++ s2 |> String.length |> Expect.equal (String.length s1 + String.length s2)
            ]
        ]
