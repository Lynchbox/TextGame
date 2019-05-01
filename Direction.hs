module Direction
( Direction(..)
, directionFromString
) where

import Data.Char (toLower)

data Direction = North | NorthEast | East | SouthEast 
               | South | SouthWest | West | NorthWest
               deriving (Show, Eq, Ord)

directionFromString :: String -> Maybe Direction
directionFromString s
  | d `elem` ["n", "north"] = return North
  | d `elem` ["ne", "northeast", "north east"] = return NorthEast
  | d `elem` ["e", "east"] = return East
  | d `elem` ["se", "southeast", "south east"] = return SouthEast
  | d `elem` ["s", "south"] = return South
  | d `elem` ["sw", "southwest", "south west"] = return SouthWest
  | d `elem` ["w", "west"] = return West
  | d `elem` ["nw", "northwest", "north west"] = return NorthWest
  | otherwise = Nothing
  where d = map toLower s    