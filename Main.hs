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
travel d = state $ \g -> fromMaybe ("You get an uncomfortable feeling going this way, maybe you should think again?", g) --message displayed if wrong way
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
  | s `elem` ["icey map", "Icey map"] = return "The ice map seems to lead to the east" -- reads the map
  | s `elem` ["ashy map", "Ashy map"] = return "The ashy map seems to lead to the north" -- reads the map
  | s `elem` ["bark map", "Bark map"] = return "The bark map seems to lead to the north west" -- reads the map
  | otherwise = return "Not a command" -- if none of these are entered displays error message
  where
    direction = directionFromString s