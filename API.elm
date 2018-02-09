



type MapAction
  = Map_NewTileLayer TileLayer
  | Map_NewVectorLayer VectorLayer
  | Map_UpdateCenter InitialCoords
  | 

map = { defaultMap |
        center = {lat=36.1, lng=43.1 
      }

-- Need a way to add layers to existing map

updateMap : MapAction -> Map -> Map
updateMap


-- Idea:
-- Let uses specify their own views for TileLayer/Vector Layer

