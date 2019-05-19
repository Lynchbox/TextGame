module Game 
( Game(..)
, GameState
, find
, enterRoom
, gameData
, initGame
, takeItem
, displayInv
) where

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Data.Maybe (fromMaybe)

import Direction

-- imports the other modules codes
import Item (Item)
import qualified Item as Item

import Room (Room)
import qualified Room as Room

type GameState = State Game String

data Game = Game
  { currentRoom :: Room
  , roomMap :: Map String Room
  , inventory :: [Item]
  } deriving Show
--Room building code
gameData = Game { roomMap = Map.fromList
  [("hall",
    Room.Room { Room.name = "Hall of Gods"
         , Room.description = "You wake in the middle of a gigantic hall. You're startled when a deep voice projects into your mind !Do not be Alarmed Traveller for this is but a dream realm used for taking the test of the gods, >take< the >lamp< infront of you and begin your test through the Southern door!"
         , Room.directions = Map.fromList [
             (South, "cavern") ]
         , Room.visited = False
         , Room.items = [
-- Item list
              Item.Item { Item.name = "lamp"} ]
         })
    ,("cavern",
    Room.Room { Room.name = "Cavern"
         , Room.description = "You open the large door and find yourself in a large cavern. By a door to the >w<est an old map hangs, to the >s<outh there's another door covered in vines"
         , Room.directions = Map.fromList [
             (South, "forest") ]
         , Room.visited = False
         , Room.items = [
             Item.Item { Item.name = "map" } ]
         })
    ,("forest",
      Room.Room { Room.name = "Forest"
         , Room.description = "You open the large door and find yourself in a large cavern. By a door to the >w<est an old map hangs, to the >s<outh there's another door covered in vines"
         , Room.directions = Map.fromList [
             (North, "") ]
         , Room.visited = False
         , Room.items = [
              Item.Item { Item.name = "adsd" } ]
         })
  ]
  , currentRoom = undefined
  , inventory = []
  }

find :: String -> Game -> Room
find n g = (roomMap g) ! n

enterRoom :: String -> GameState
enterRoom n =
  state $ \g ->
    (msg g
    , g { roomMap = Map.update (\x -> Just (x { Room.visited = True })) n (roomMap g),
       currentRoom = r g })
  where 
    r g = find n g
    msg g
      | Room.visited (r g) = Room.name (r g)
      | otherwise = Room.nameWithDescription (r g)
--when the game starts load into hall first
initGame :: GameState
initGame = enterRoom "hall"
--When an item is taken removes it from the room
removeItemFromCurrentRoom :: Item -> GameState
removeItemFromCurrentRoom i = state $ \g ->
  ("Taken!", g { currentRoom = Room.removeItem i (currentRoom g), inventory = i:(inventory g) })
--takes item or returns item not found
takeItem :: String -> GameState
takeItem n = state $ \g -> fromMaybe ("Item not found", g)
  (flip runState g . removeItemFromCurrentRoom
    <$> Room.findItem n (currentRoom g))
--inventory display
displayInv :: GameState
displayInv = state $ \g -> (safeInit . unlines . fmap Item.name . inventory $ g , g)
  where safeInit [] = []
        safeInit xs = init xs