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

gameData = Game { roomMap = Map.fromList
  [("corridor",
    Room.Room { Room.name = "Corridor"
         , Room.description = "ayyyy"
         , Room.directions = Map.fromList [
             (North, "dining hall") ]
         , Room.visited = False
         , Room.items = []
        })
  ,("dining hall",
    Room.Room { Room.name = "Dining Hall"
         , Room.description = "good food is here"
         , Room.directions = Map.fromList [
             (South, "corridor") ]
         , Room.visited = False
         , Room.items = [
             Item.Item { Item.name = "apple" } ]
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

initGame :: GameState
initGame = enterRoom "corridor"

removeItemFromCurrentRoom :: Item -> GameState
removeItemFromCurrentRoom i = state $ \g ->
  ("Taken!", g { currentRoom = Room.removeItem i (currentRoom g), inventory = i:(inventory g) })

takeItem :: String -> GameState
takeItem n = state $ \g -> fromMaybe ("Item not found", g)
  (flip runState g . removeItemFromCurrentRoom
    <$> Room.findItem n (currentRoom g))

displayInv :: GameState
displayInv = state $ \g -> (safeInit . unlines . fmap Item.name . inventory $ g , g)
  where safeInit [] = []
        safeInit xs = init xs