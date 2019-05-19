import System.IO (hFlush, stdout)
import Control.Monad (unless, when, guard)
import Control.Applicative ((<|>), (<*>))
import Data.Maybe (fromMaybe, isJust, fromJust)
import Control.Monad.State
import Data.Char (isSpace)

import Room (Room)
import qualified Room as Room
import Game (Game, GameState)
import qualified Game as Game
import Direction

import Item (Item)
import qualified Item as Item

type GameResponse = IO (String, Game)
--stops overflow exceptions 
trim :: String -> String
trim = foldr pickChars []
  where
    pickChars ' ' (' ':xs) = ' ':xs
    pickChars c1 (x:xs)  = c1:x:xs
    pickChars c1 [] = [c1]

travel :: Direction -> GameState -- direction control
travel d = state $ \g -> fromMaybe ("Your path is blocked", g) --message displayed if no room exists
  (flip runState g . Game.enterRoom <$>
    Room.roomInDirection d (Game.currentRoom g))

main = do -- starts the game
  (msg, _) <- play . return $ runState Game.initGame Game.gameData
  putStrLn msg

play :: GameResponse -> GameResponse -- quit game code
play x = do
  (msg, gameData) <- x
  unless (null msg) $ putStrLn msg >> putStrLn ""
  putStr "> "
  hFlush stdout
  response <- trim <$> getLine
  if response `elem` ["q", "quit"]
    then return ("Bye!", gameData)
    else play . return $ runState (exec response) gameData

exec :: String -> GameState
exec "look" = state $ \g -> (Room.nameWithDescription $ Game.currentRoom g, g) -- displays room description and name
exec s
  | isJust direction = travel (fromJust direction) --move in the inputted direction
  | take 4 s == "take" || take 3 s == "get" = Game.takeItem s --takes the item from the room
  | s `elem` ["i", "inv", "inventory"] = Game.displayInv -- displays players inventory
  | s `elem` ["m", "map"] = return "The Map is old and barely readable, but you can make out the feignt directions of south, east, south east, ,west, north would lead you to a small room with a diamond in the middle" 
  | otherwise = return "Not a command" -- if none of these are entered displays error message
  where
    direction = directionFromString s