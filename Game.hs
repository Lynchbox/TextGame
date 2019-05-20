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
  [("tutorial",
       Room.Room { Room.name = "Tutorial"
         , Room.description = "Welcome to my riddle filled text based game! to move in directions use the inputs n,s,e,w,ne,nw,se,sw. To pickup items use take >item name< items will be highlighted with >< to read the maps you collect simply input >map name< to check your inventory simply input i and use quit to stop the game. Start your adventure by going south and enjoy!"
         , Room.directions = Map.fromList [
             (South, "hall") ]
         , Room.visited = False
         , Room.items = []
         })
    ,("hall",
    Room.Room { Room.name = "Hall of Gods"
         , Room.description = "You wake in the middle of a gigantic hall. You're startled when a deep voice projects into your mind !Do not be Alarmed Traveller for this is but a dream realm used for taking the test of the gods, take the >lamp< infront of you and begin your test through the Southern door!"
         , Room.directions = Map.fromList [
             (South, "chamber") ]
         , Room.visited = False
         , Room.items = [
-- Item list
          Item.Item { Item.name = "lamp"} ]
         })
    ,("chamber",
      Room.Room { Room.name = "Golden chamber"
         , Room.description = "You open the large door and find yourself in a small golden chamber with a statue of what looks like a minotaur in the middle, 4 doors surround the chamber, the south eastern one has a crown on it, the eastern one has sword on it, the western one has a skull on it and the south western has an hour glass on it. !Ah another poor soul sent to answer my riddle or be lost here forever enter the door correlating to the answer or spend eternity inside this lost chamber. This thing all things devours birds, beasts, trees, flowers Gnaws iron bites steel, grinds hard stones to meal slays kings ruins towns and beats high mountains down! "
         , Room.directions = Map.fromList [
             (West, "cavern") ]
         , Room.visited = False
         , Room.items = []
         })
    ,("cavern",
      Room.Room { Room.name = "Cavern"
         , Room.description = "A bright light flashes in your eyes as you enter the door of time. On the wall is a drawing of an incomplete map with 3 missing pieces. The current pieces lead you north east !So you managed to pass through the door of time hmm? Well if you wish to find your way home you'll need to complete that map. Each piece is locked behind a riddle belonging to its world! To the south is a door with a tree on it, to the north there's a door with a snowflake to the south west the door has a volcanoe on it"
         , Room.directions = Map.fromList [
             (South, "forest"), (North, "iceland"), (SouthWest, "fireland"), (NorthEast, "cavern2") ]  
         , Room.visited = False
         , Room.items = []
         })
    ,("iceland",
      Room.Room { Room.name = "Ice plateau"
         , Room.description = "As you walk through the door a blistering cold wind instantly hits you All you can see for miles is snow other than 3 ice covered doors, the southern one has an icicle on it, the western one a wolf and the eastern one a giant snowball . In the wind you suddenly make out a voice !Enter the correct door to claim your map piece or freeze here forever. Glittering points that downward thrust, sparkling spears, that never rust!"
         , Room.directions = Map.fromList [
             (South, "map1") ]
         , Room.visited = False
         , Room.items = []
         })
    ,("map1",
      Room.Room { Room.name = "Icey Cave"
         , Room.description = "The blistering winds leave you as you enter through the door. On the ground in front of you lays an >icey map<. !Well done human go back through the door whence you came to be returned to the map cavern!"
         , Room.directions = Map.fromList [
             (North, "cavern") ]
         , Room.visited = False
         , Room.items = [
             Item.Item { Item.name = "icey map" } ]
         })
    ,("forest",
      Room.Room { Room.name = "Forest"
         , Room.description = "The sweet air and sounds of a forest fills your senses as you walk through the door. You notice 3 wooden doors, one to the south east with leaves on it another to north east with an Oak tree on it and the last to the north west with a bird on it. On a large tree infront of you a riddle is carved !Walk on the living they don't even mumble, walk on the dead they mutter and grumble. Make your choice wisley!"
         , Room.directions = Map.fromList [
             (SouthEast, "map2") ]
         , Room.visited = False
         , Room.items = []
         })
    ,("map2",
      Room.Room { Room.name = "Wooden hut"
         , Room.description = "You walk into what appears to be a small wooden hut. On a table in front of you lays a >bark map<. A door suddenly appears on the southern wall !Well done human go take this door to be returned to the map cavern!"
         , Room.directions = Map.fromList [
             (South, "cavern") ]
         , Room.visited = False
         , Room.items = [
             Item.Item { Item.name = "bark map" } ]
         })
    ,("fireland",
      Room.Room { Room.name = "Firelands"
         , Room.description = "A gust of sweltering hot air smashes against your body as step through the door. The entire plane is covered in molten lava and fire geysers. You look around and see 3 small volcanoe like structures with doors leading into them, one to the south east with a picture of a volcanoe on it, one to the south west with a picture of a flame on it and one directly south with a burning man. Suddenly the fire on the ground starts to spell out a riddle !As destructive as life, as healing as death, an institutioner of strife, Just as prone to bless, It is all that is good, Yet with an evil trend, As it was the beginning of things, It can also be the end."
         , Room.directions = Map.fromList [
             (SouthWest, "map3") ]
         , Room.visited = False
         , Room.items = []
         })
    ,("map3",
      Room.Room { Room.name = "Lava room"
         , Room.description = "It becomes harder to breath as you enter a room with lava running down the walls an >ashy map< is sat on a pedestal in the middle. !BAGH take the map and leave this place before my fire consumes you! A door appears to the east as the lava starts seeping closer to you"
         , Room.directions = Map.fromList [
             (South, "cavern") ]
         , Room.visited = False
         , Room.items = [
             Item.Item { Item.name = "ashy map" } ]
         })
     ,("cavern2",
       Room.Room { Room.name = "Firey cave"
         , Room.description = "You walk through the door to enter a cave with fires all along the wall and 3 doors one to the south east, north east and east. Maybe one of the maps you collected would help here?  "
         , Room.directions = Map.fromList [
             (North, "cavern3") ]
         , Room.visited = False
         , Room.items = []
         })
     ,("cavern3",
       Room.Room { Room.name = "Icey cave"
         , Room.description = "You follow the map into a freezing cave. Doors to the south, west and east suddenly appear"
         , Room.directions = Map.fromList [
             (East, "cavern4") ]
         , Room.visited = False
         , Room.items = []
         })
     ,("cavern4",
       Room.Room { Room.name = "Forest cave"
         , Room.description = "Once again you follow the map this time leading you into a cave covered in vines and greenery. through the vines you make out soors to the south east,  north west and north east"
         , Room.directions = Map.fromList [
             (NorthWest, "Hall2") ]
         , Room.visited = False
         , Room.items = []
         })
     ,("Hall2",
       Room.Room { Room.name = "Hall of Gods"
         , Room.description = "After going through the last door a sudden wave of drowsiness hit you. You awake to loud laughter. !HAHAHA EXCELLENT you used your witts and managed to pass our tests congratulations your freedom and our favour are yours! "
         , Room.directions = Map.fromList [
             (East, "") ]
         , Room.visited = False
         , Room.items = []
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