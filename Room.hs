module Room
( Room(..)
, roomInDirection
, look
, nameWithDescription
, removeItem
, findItem
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (find, isInfixOf, delete)

import Direction

import Item (Item)
import qualified Item as Item

data Room = Room -- room variables
  { name :: String
  , description :: String
  , directions :: Map Direction String
  , visited :: Bool
  , items :: [Item]
  } deriving (Show, Eq)

roomInDirection :: Direction -> Room -> Maybe String -- moves to room in inputed direction
roomInDirection d r = Map.lookup d (directions r)

nameWithDescription :: Room -> String -- room name + description
nameWithDescription r = name r ++ '\n':(description r)

look :: Room -> IO () -- look command
look = putStrLn . nameWithDescription

findItem :: String -> Room -> Maybe Item -- checks if an item is set for the room 
findItem n = find (\i -> isInfixOf (Item.name i) n) . items

removeItem :: Item -> Room -> Room -- removes item from room
removeItem i r = r { items = delete i (items r) }