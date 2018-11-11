{-# OPTIONS_GHC -Wall #-}

module Lib where

import Data.Fixed (div')
import qualified Data.Set as S
import Graphics.Gloss

-- ------------------------ Game types ------------------------ --

-- | Tile of level.
data Tile = Ground | Brick | BonusBlockActive | BonusBlockEmpty | Empty

-- | Level of the game.
type Level = [[Tile]]

-- | Types of possible player input
data Movement = UP_BUTTON | DOWN_BUTTON | LEFT_BUTTON | RIGHT_BUTTON | SPECIAL_BUTTON deriving (Eq, Ord, Show)

-- | State of the game (HP levelNumber nextLevel).
data GameState = GameState
  { gameStateHp          :: Int
  , gameStateLvlNum      :: Int
  , gameStateNextLvlNum  :: Maybe Int
  , pressedKeys          :: S.Set Movement
  }

--   Game        Levels  Player       Level Objects  State
data Game = Game [Level] MovingObject [MovingObject] GameState

-- Objects
type Vector2 = (Float, Float)
type Coord = (Integer, Integer)
type Position = Vector2
type Velocity = Vector2
type Acceleration = Vector2
type Size = Vector2

-- | Data type for the objects of the level.
data MovingObject
  = MovingObject Kind Position Velocity Acceleration

-- | Kind of the MovingObject.
data Kind
  -- Player
  = BigPlayer
  | SmallPlayer
  -- Enemies
  | Gumba
  | Turtle
  -- Items
  | Mushroom
  | Star
  | Shell

-- | Types of collisions.
data CollisionType = Delete | Spawn Kind Coord | Change Tile | Bounce

-- | Container with textures for objects.
data Assets = Assets
  { marioSprites :: [Picture]
  , envSprites   :: [Picture]
  , enemySprites :: [Picture]
  }

-- ------------------------ Game scale ------------------------ --

-- | Size of the tiles.
tileSize :: Float
tileSize = 16

-- | Size of the minimum MovingObject.
-- Size of the others should be a scalar multiplication of this.
minObjSize :: Float
minObjSize = 0.8 * tileSize

-- | Size of the text.
textScaleFactor :: Float
textScaleFactor = 0.008 * tileSize

-- | Game scale.
gameScaleFactor :: Float
gameScaleFactor = 1

-- ------------------------ Game constants ------------------------ --

-- | Gravity of the world.
g :: Float
g = 9 * tileSize

-- | Step of Player (speed).
step :: Vector2
step = (0.3 * tileSize, g)

-- | Thresh of collision distance.
-- If collisions doesn't work play with it.
thresh :: Float
thresh = 0.05 * tileSize

-- | Can Objects move through this tile?
canPass :: Tile -> Bool
canPass Empty = True
canPass _ = False

-- | Friction rate of the tiles.
tileFrictionRate :: Tile -> Float
tileFrictionRate Empty = 0.04
tileFrictionRate _ = 0.03

-- | Type of collision with player.
typeOfCollision :: Tile -> [CollisionType]
typeOfCollision Brick = [Delete, Bounce]
typeOfCollision BonusBlockActive
  = [Spawn Mushroom (0, 1), Change BonusBlockEmpty, Bounce]
typeOfCollision Empty = []
typeOfCollision _ = [Bounce]

-- | Get size of `MovingObject of given kind.
getSize :: Kind -> Size
getSize BigPlayer =   (minObjSize, minObjSize * 2)
getSize SmallPlayer = (minObjSize, minObjSize)
getSize Gumba =       (minObjSize, minObjSize)
getSize Turtle =      (minObjSize, minObjSize * 2)
getSize Mushroom =    (minObjSize, minObjSize)
getSize Star =        (minObjSize, minObjSize)
getSize Shell =       (minObjSize, minObjSize)

-- ------------------------ Game initialization ------------------------ --

-- | Init state of the game.
initGame :: [Level] -> Game
initGame levels = Game levels initPlayer initObjects initState

-- | Init state of the game.
initState :: GameState
initState =  GameState
  { gameStateHp = 3
  , gameStateLvlNum = 0
  , gameStateNextLvlNum = Nothing
  , pressedKeys = S.empty
  }

-- | Initial state of the player.
initPlayer :: MovingObject
initPlayer = MovingObject SmallPlayer (2 * tileSize, 2 * tileSize) (0.0, 0.0) (0.0, 0.0)

-- | Initial amount of enemies.
initObjects :: [MovingObject]
initObjects =
  [ MovingObject Gumba (21 * tileSize, 2 * tileSize) (-1.0 * tileSize, 0.0) (0.0, 0.0)
  , MovingObject Gumba (41 * tileSize, 2 * tileSize) (-1.0 * tileSize, 0.0) (0.0, 0.0)
  , MovingObject Gumba (54 * tileSize, 2 * tileSize) (-1.0 * tileSize, 0.0) (0.0, 0.0)
  , MovingObject Gumba (56 * tileSize, 2 * tileSize) (-1.0 * tileSize, 0.0) (0.0, 0.0)
  , MovingObject Gumba (81 * tileSize, 10 * tileSize) (-1.0 * tileSize, 0.0) (0.0, 0.0)
  , MovingObject Gumba (83 * tileSize, 10 * tileSize) (-1.0 * tileSize, 0.0) (0.0, 0.0)
  , MovingObject Gumba (98 * tileSize, 4 * tileSize) (-1.0 * tileSize, 0.0) (0.0, 0.0)
  , MovingObject Gumba (100 * tileSize, 4 * tileSize) (-1.0 * tileSize, 0.0) (0.0, 0.0)
  , MovingObject Gumba (116 * tileSize, 2 * tileSize) (-1.0 * tileSize, 0.0) (0.0, 0.0)
  , MovingObject Gumba (118 * tileSize, 2 * tileSize) (-1.0 * tileSize, 0.0) (0.0, 0.0)
  , MovingObject Gumba (125 * tileSize, 2 * tileSize) (-1.0 * tileSize, 0.0) (0.0, 0.0)
  , MovingObject Gumba (127 * tileSize, 2 * tileSize) (-1.0 * tileSize, 0.0) (0.0, 0.0)
  , MovingObject Gumba (130 * tileSize, 2 * tileSize) (-1.0 * tileSize, 0.0) (0.0, 0.0)
  , MovingObject Gumba (175 * tileSize, 2 * tileSize) (-1.0 * tileSize, 0.0) (0.0, 0.0)
  , MovingObject Gumba (177 * tileSize, 2 * tileSize) (-1.0 * tileSize, 0.0) (0.0, 0.0)
  , MovingObject Turtle (107 * tileSize, 2 * tileSize) (-1.0 * tileSize, 0.0) (0.0, 0.0)
  ]

-- ------------------------ Work with map ------------------------ --

-- | Safely take the tile with given indexes from the level.
takeElemFromMatrix :: [[a]] -> Coord -> Maybe a
takeElemFromMatrix [] (_, _) = Nothing
takeElemFromMatrix (l:_) (pos_x, 0) = takeElemFromList l pos_x
takeElemFromMatrix (_:ls) (pos_x, pos_y) = takeElemFromMatrix ls (pos_x, pos_y - 1)

-- | Safely take tile from the tile row.
takeElemFromList :: [a] -> Integer -> Maybe a
takeElemFromList [] _ = Nothing
takeElemFromList (l:_) 0 = Just l
takeElemFromList (_:ls) n = takeElemFromList ls (n - 1)

-- | Update the level in the list.
updateLvls :: [Level] -> Int -> Coord -> Tile -> [Level]
updateLvls [] _ _ _ = []
updateLvls (l:ls) 0 pos tile = updateLvl l pos tile : ls
updateLvls (l:ls) n pos tile = l : updateLvls ls (n - 1) pos tile

-- | Update the tile in the level.
updateLvl :: Level -> Coord -> Tile -> Level
updateLvl [] _ _ = []
updateLvl (l:ls) (pos_x, 0) tile = updateRow l pos_x tile : ls
updateLvl (l:ls) (pos_x, pos_y) tile = l : updateLvl ls (pos_x, pos_y - 1) tile

-- | Update the tile in the row.
updateRow :: [Tile] -> Integer -> Tile -> [Tile]
updateRow [] _ _ = []
updateRow (_:ls) 0 tile = tile : ls
updateRow (l:ls) n tile = l : updateRow ls (n - 1) tile

-- | Translate position to the coords for the map.
mapPosToCoord :: Position -> Coord
mapPosToCoord (x, y) = (div' x tileSize, div' y tileSize)

-- | Translate position to the coords for the map.
mapCoordToPos :: Coord -> Position
mapCoordToPos (x, y)
  = (fromIntegral x * tileSize, fromIntegral y * tileSize)

-- | Checks the given bool exression for all parts of given body size.
-- Returns `True` only if all body parts satisfy given expression.
checkforAllParts :: (Level -> Position -> Bool)
                 -> Level -> Size -> Position -> Bool
checkforAllParts = applyToParts (&&) True

-- | Checks the given bool exression for all parts of given body size.
-- Returns `True` if at least one body part satisfy given expression.
checkForAnyPart :: (Level -> Position -> Bool)
                -> Level -> Size -> Position -> Bool
checkForAnyPart = applyToParts (||) False

-- | Applies the given function to all parts of given body.
applyToParts
  :: (b -> b -> b)            -- ^ Function to use in `fold` operation
  -> b                        -- ^ Neutral element for `fold` operation
  -> (Level -> Position -> b) -- ^ Function for taking elem from level map
  -> Level                    -- ^ Level map
  -> Size                     -- ^ Size of the body
  -> Position                 -- ^ Placement of the body on the map
  -> b
applyToParts funForFold base posFun lvl size (pos_x, pos_y)
  = foldr funForFold base
  (map (\(x, y) -> posFun lvl (pos_x + offset x, pos_y + offset y))
    [(a, b) | a <- [0..count_x], b <- [0..count_y]])
  where
    (count_x, count_y) = mapPosToCoord size
    offset n = fromIntegral n * minObjSize
