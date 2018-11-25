{-# OPTIONS_GHC -Wall #-}

module Lib where

import Data.Fixed (div')
import qualified Data.Set as S
import Graphics.Gloss

-- ------------------------ Game types ------------------------ --

-- | Tile of level.
data Tile
  = Ground
  | Brick
  | BrickCoinBlock
  | BrickStarBlock
  | BonusBlockCoin
  | BonusBlockPowerUp
  | HiddenBlockLivesUp
  | BonusBlockEmpty
  | Empty
  | PipeGreenTopLeft
  | PipeGreenTopRight
  | PipeGreenLeft
  | PipeGreenRight
  | RomboBlock
  | Coin

-- | Map of the level.
type LevelMap = [[Tile]]

-- | Level of the game.
data Level = Level
  { levelMap :: LevelMap
  , levelObjs :: [MovingObject]
  , levelInitPoint :: Coord
  }

-- | Types of possible player input
data Movement = UP_BUTTON | DOWN_BUTTON | LEFT_BUTTON | RIGHT_BUTTON | SPECIAL_BUTTON
  | W_BUTTON | A_BUTTON | D_BUTTON
  deriving (Eq, Ord, Show)

-- | State of the game (HP levelNumber nextLevel).
data GameState = GameState
  { gameStateHp          :: Int
  , gameStateCoins       :: Int
  , gameStateLvlNum      :: Int
  , gameStateNextLvlNum  :: Maybe Int
  , pressedKeys          :: S.Set Movement
  }

--   Game        Levels  Players       State
data Game = Game [Level] [MovingObject] GameState

-- Objects
type Vector2 = (Float, Float)
type Coord = (Integer, Integer)
type Position = Vector2
type Velocity = Vector2
type Acceleration = Vector2
type Size = Vector2

-- | Data type for the objects of the level.
data MovingObject = MovingObject Kind Position Velocity Acceleration Float Int

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
  | HpMushroom
  | Star
  | Shell

-- | Types of collisions.
data CollisionType = Delete | Spawn Kind Coord | Change Tile | Bounce | CollectCoin

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
gameScaleFactor = 1/2

-- | Speed of animation.
animationScale :: Float
animationScale = 6

-- ------------------------ Game constants ------------------------ --

-- | Gravity of the world.
g :: Float
g = 25 * tileSize

-- | Step of Player (speed).
step :: Vector2
step = (0.3 * tileSize, 0.6 * g)

-- | Thresh of collision distance.
-- If collisions doesn't work play with it.
thresh :: Float
thresh = 0.05 * tileSize

-- | Is this object a player?
isPlayer :: Kind -> Bool
isPlayer BigPlayer = True
isPlayer SmallPlayer = True
isPlayer _ = False

-- | Can Objects move through this tile?
canPass :: Tile -> Bool
canPass Empty = True
canPass HiddenBlockLivesUp = True
canPass _ = False

-- | Determines the speed of animation for different kinds of objects.
getAnimDivisor :: Kind -> Float
getAnimDivisor _ = 1000

-- | Friction rate of the tiles.
tileFrictionRate :: Tile -> Float
tileFrictionRate Empty = 0.04
tileFrictionRate _ = 0.03

-- | Type of collision with player.
typeOfCollision :: Tile -> [CollisionType]
typeOfCollision Brick = [Delete, Bounce]
typeOfCollision BrickCoinBlock
  = [CollectCoin, Change BonusBlockEmpty, Bounce]
typeOfCollision BrickStarBlock
  = [Spawn Star (0, 1), Change BonusBlockEmpty, Bounce]
typeOfCollision BonusBlockCoin
  = [CollectCoin, Change BonusBlockEmpty, Bounce]
typeOfCollision BonusBlockPowerUp
  = [Spawn Mushroom (0, 1), Change BonusBlockEmpty, Bounce]
typeOfCollision HiddenBlockLivesUp
  = [Spawn HpMushroom (0, 1), Change BonusBlockEmpty, Bounce]
typeOfCollision Empty = []
typeOfCollision _ = [Bounce]

-- | Get size of `MovingObject of given kind.
getSize :: Kind -> Size
getSize BigPlayer =   (minObjSize, minObjSize * 2)
getSize SmallPlayer = (minObjSize, minObjSize)
getSize Gumba =       (minObjSize, minObjSize)
getSize Turtle =      (minObjSize, minObjSize * 2)
getSize Mushroom =    (minObjSize, minObjSize)
getSize HpMushroom =  (minObjSize, minObjSize)
getSize Star =        (minObjSize, minObjSize)
getSize Shell =       (minObjSize, minObjSize)

getInitSpeed :: Kind -> Vector2
getInitSpeed BigPlayer =   (0, 0)
getInitSpeed SmallPlayer = (0, 0)
getInitSpeed Gumba =       (-1 * tileSize, 0)
getInitSpeed Turtle =      (-1 * tileSize, 0)
getInitSpeed Mushroom =    (-1 * tileSize, 0)
getInitSpeed HpMushroom =  (-1 * tileSize, 0)
getInitSpeed Star =        (-1 * tileSize, g)
getInitSpeed Shell =       (-1 * tileSize, 0)

-- ------------------------ Game initialization ------------------------ --

-- | Init state of the game.
initGame :: [Level] -> Game
initGame levels = 
  Game levels 
    ([initPlayer (levelInitPoint (levels !! gameStateLvlNum initState))
     ,initPlayer (levelInitPoint (levels !! gameStateLvlNum initState))]) initState

-- | Init state of the game.
initState :: GameState
initState =  GameState
  { gameStateHp = 3
  , gameStateCoins = 0
  , gameStateLvlNum = 0
  , gameStateNextLvlNum = Nothing
  , pressedKeys = S.empty
  }

-- | Initial state of the player.
initPlayer :: Coord -> MovingObject
initPlayer (coord_x, coord_y)
  = MovingObject SmallPlayer
    (fromIntegral coord_x * tileSize, fromIntegral coord_y * tileSize) (0.0, 0.0) (0.0, 0.0) 0 5

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
updateLvlMap :: [Level] -> Int -> Coord -> Tile -> [Level]
updateLvlMap [] _ _ _ = []
updateLvlMap (l:ls) 0 pos tile
  = l { levelMap = updateElemInmatrix (levelMap l) pos tile } : ls
updateLvlMap (l:ls) n pos tile = l : updateLvlMap ls (n - 1) pos tile

-- | Update the tile in the level.
updateElemInmatrix :: [[a]] -> Coord -> a -> [[a]]
updateElemInmatrix [] _ _ = []
updateElemInmatrix (l:ls) (pos_x, 0) tile
  = updateElemInList l tile pos_x : ls
updateElemInmatrix (l:ls) (pos_x, pos_y) tile
  = l : updateElemInmatrix ls (pos_x, pos_y - 1) tile

-- | Update element in list.
updateElemInList :: [a] -> a -> Integer -> [a]
updateElemInList [] _ _ = []
updateElemInList (_:xs) newElem 0 = newElem : xs
updateElemInList (x:xs) newElem n = x
  : updateElemInList xs newElem (n - 1)

-- | Translate position to the coords for the map.
mapPosToCoord :: Position -> Coord
mapPosToCoord (x, y) = (div' x tileSize, div' y tileSize)

-- | Translate position to the coords for the map.
mapCoordToPos :: Coord -> Position
mapCoordToPos (x, y)
  = (fromIntegral x * tileSize, fromIntegral y * tileSize)

-- | Checks the given bool exression for all parts of given body size.
-- Returns `True` only if all body parts satisfy given expression.
checkforAllParts :: (LevelMap -> Position -> Bool)
                 -> LevelMap -> Size -> Position -> Bool
checkforAllParts = applyToParts (&&) True

-- | Checks the given bool exression for all parts of given body size.
-- Returns `True` if at least one body part satisfy given expression.
checkForAnyPart :: (LevelMap -> Position -> Bool)
                -> LevelMap -> Size -> Position -> Bool
checkForAnyPart = applyToParts (||) False

-- | Applies the given function to all parts of given body.
applyToParts
  :: (b -> b -> b)               -- ^ Function to use in `fold` operation
  -> b                           -- ^ Neutral element for `fold` operation
  -> (LevelMap -> Position -> b) -- ^ Function for taking elem from level map
  -> LevelMap                    -- ^ Level map
  -> Size                        -- ^ Size of the body
  -> Position                    -- ^ Placement of the body on the map
  -> b
applyToParts funForFold base posFun lvl size (pos_x, pos_y)
  = foldr funForFold base
  (map (\(x, y) -> posFun lvl (pos_x + offset x, pos_y + offset y))
    [(a, b) | a <- [0..count_x], b <- [0..count_y]])
  where
    (count_x, count_y) = mapPosToCoord size
    offset n = fromIntegral n * minObjSize
