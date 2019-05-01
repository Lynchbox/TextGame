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

trim :: String -> String
trim = foldr pickChars []
  where
    pickChars ' ' (' ':xs) = ' ':xs
    pickChars c1 (x:xs)  = c1:x:xs
    pickChars c1 [] = [c1]

travel :: Direction -> GameState
travel d = state $ \g -> fromMaybe ("placeholder", g)
  (flip runState g . Game.enterRoom <$>
    Room.roomInDirection d (Game.currentRoom g))

main = do
  (msg, _) <- play . return $ runState Game.initGame Game.gameData
  putStrLn msg

play :: GameResponse -> GameResponse
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
exec "look" = state $ \g -> (Room.nameWithDescription $ Game.currentRoom g, g)
exec s
  | isJust direction = travel (fromJust direction)
  | take 4 s == "take" || take 3 s == "get" = Game.takeItem s
  | s `elem` ["i", "inv", "inventory"] = Game.displayInv
  | s `elem` [" ", ""] = return ""
  | otherwise = return "Not a command"
  where
    direction = directionFromString s