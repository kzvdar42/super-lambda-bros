{-# OPTIONS_GHC -Wall #-}

module Lib where

import Data.Fixed (div')
import qualified Data.Set as S
import Graphics.Gloss

-- ------------------------ Game types ------------------------ --

-- | Tile of level.
data Tile
  = Ground
  | TopBrick | MiddleBrick | BrickCoinBlock | BrickStarBlock
  | BonusBlockCoin | BonusBlockPowerUp | HiddenBlockLivesUp | BonusBlockEmpty
  | PipeGreenTopLeft | PipeGreenTopRight | PipeGreenLeft | PipeGreenRight
  | RomboBlock
  | Coin
  | Empty

-- | Map of the level.
type LevelMap = [[Tile]]

-- | Level of the game.
data Level = Level
  { levelMap :: LevelMap
  , levelObjs :: [MovingObject]
  , levelSprites  :: [Sprite]
  , levelInitPoint :: Coord
  }

-- | Types of possible player input.
data Movement
  -- | First player.
  = P1_U_BUTTON | P1_L_BUTTON | P1_D_BUTTON | P1_R_BUTTON
  -- | Second player.
  | P2_U_BUTTON | P2_L_BUTTON | P2_D_BUTTON | P2_R_BUTTON
  -- | Third player.
  | P3_U_BUTTON | P3_L_BUTTON | P3_D_BUTTON | P3_R_BUTTON
  | ENTER_BUTTON
  deriving (Eq, Ord, Show)

-- | Player state.
data Player = Player
  { playerObj    :: MovingObject -- ^ MovingObject of a player
  , playerHp     :: Integer      -- ^ Player Hp
  , playerIsDead :: Bool         -- ^ Is player dead?
  }

-- | Current state of the game.
data Game = Game
    { gameLevels     :: [Level]        -- ^ Levels
    , gameCurLevel   :: Level          -- ^ Current level
    , gamePlayers    :: [Player]       -- ^ Player
    , gameCoins      :: Integer        -- ^ Number of coins
    , gameLvlNum     :: Int            -- ^ Current level number
    , gameNextLvlNum :: Maybe Int      -- ^ Next level number
    , pressedKeys    :: S.Set Movement -- ^ List of current pressed keys
    }

-- Objects
type ScreenSize = (Int, Int)
type Vector2 = (Float, Float)
type Coord = (Integer, Integer)
type Position = Vector2
type Velocity = Vector2
type Acceleration = Vector2
type Size = Vector2

-- | Data type for the objects of the level.
data MovingObject = MovingObject 
  { objectKind  :: Kind         -- ^ Object kind
  , objectPos   :: Position     -- ^ Object position
  , objectVel   :: Velocity     -- ^ Object velocity
  , objectAccel :: Acceleration -- ^ Object acceleration
  , objectAnimC :: Float        -- ^ Object animation ???
  , objectAnimD :: Int          -- ^ Object animation ???
  }

-- | Data type for the animation handling with short time-to-live sprites
data Sprite =
  Sprite SpriteType Position Float Float [OnSpriteDestroy]

-- | Type of sprite define how given sprite is rendered
data SpriteType = CoinSprite

-- | Actions that can be performed after sprite is destroyed
data OnSpriteDestroy = SelfDestroy

-- | Kind of the MovingObject.
data Kind
  -- Player
  = BigPlayer Int
  | SmallPlayer Int
  -- Enemies
  | Gumba
  | Turtle
  -- Items
  | Mushroom
  | HpMushroom
  | Star
  | Shell
  | Flagpole

-- | Types of collisions.
data CollisionType
  = DeleteTile
  | DeleteObj Int
  | SpawnObj Kind Coord
  | Change Tile
  | Bounce
  | CollectCoin
  | Die
  | ToBig
  | MoveToNextLevel
  | AddHp

-- | Container with textures for objects.
data Assets = Assets
  { marioSprites      :: [Picture]
  , luigiSprites      :: [Picture]
  , francescoSprites  :: [Picture]
  , marioSpritesB     :: [Picture]
  , luigiSpritesB     :: [Picture]
  , francescoSpritesB :: [Picture]
  , envSprites        :: [Picture]
  , enemySprites      :: [Picture]
  , animSprites       :: [Picture]
  }

-- ------------------------ Game scale ------------------------ --

-- | Size of the tiles.
tileSize :: Float
tileSize = 16

-- | Size of the minimum MovingObject.
-- Size of the others should be a scalar multiplication of this.
minObjSize :: Float
minObjSize = 0.5 * tileSize

-- | Size of the text.
textScaleFactor :: Float
textScaleFactor = 0.008 * tileSize

-- | Game scale.
gameScaleFactor :: Float
gameScaleFactor = 1

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
-- If the collisions doesn't work, play with it.
thresh :: Float
thresh = 0.10 * tileSize

-- | Is this object a player?
isPlayer :: Kind -> Bool
isPlayer (BigPlayer _)   = True
isPlayer (SmallPlayer _) = True
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
tileFrictionRate Empty = 0.02
tileFrictionRate _ = 0.015

-- | Type of collision with player.
collisionWithTile :: Tile -> Kind -> [CollisionType]
collisionWithTile TopBrick (BigPlayer _) = [DeleteTile, Bounce]
collisionWithTile TopBrick _ = [Bounce]
collisionWithTile MiddleBrick (BigPlayer _) = [DeleteTile, Bounce]
collisionWithTile MiddleBrick _ = [Bounce]
collisionWithTile BrickCoinBlock _
  = [CollectCoin, Change BonusBlockEmpty, Bounce]
collisionWithTile BrickStarBlock _
  = [SpawnObj Star (0, 1), Change BonusBlockEmpty, Bounce]
collisionWithTile BonusBlockCoin _
  = [CollectCoin, Change BonusBlockEmpty, Bounce]
collisionWithTile BonusBlockPowerUp _
  = [SpawnObj Mushroom (0, 1), Change BonusBlockEmpty, Bounce]
collisionWithTile HiddenBlockLivesUp _
  = [SpawnObj HpMushroom (0, 1), Change BonusBlockEmpty, Bounce]
collisionWithTile Empty _ = []
collisionWithTile _  _ = [Bounce]

-- | Type of collision with player.
collisionWithObject :: Kind -> Int -> ([CollisionType], [CollisionType])
collisionWithObject Gumba n = ([Die], [DeleteObj n, Bounce])
collisionWithObject Turtle n = ([Die], [DeleteObj n, SpawnObj Shell (0, 0), Bounce])
collisionWithObject Mushroom n = ([DeleteObj n, ToBig], [DeleteObj n, ToBig])
collisionWithObject HpMushroom n = ([DeleteObj n, AddHp], [DeleteObj n, AddHp])
collisionWithObject Flagpole _ = ([MoveToNextLevel], [MoveToNextLevel])
collisionWithObject _ _ = ([], [])

-- | Get size of `MovingObject of given kind.
getSize :: Kind -> Size
getSize (BigPlayer _)   = (minObjSize * 2, minObjSize * 4)
getSize (SmallPlayer _) = (minObjSize * 2, minObjSize * 2)
getSize Gumba           = (minObjSize * 2, minObjSize * 2)
getSize Turtle          = (minObjSize * 2, minObjSize * 3)
getSize Mushroom        = (minObjSize * 2, minObjSize * 2)
getSize HpMushroom      = (minObjSize * 2, minObjSize * 2)
getSize Star            = (minObjSize * 2, minObjSize * 2)
getSize Shell           = (minObjSize * 2, minObjSize * 2)
getSize Flagpole        = (1 * tileSize, 10 * tileSize)

getInitSpeed :: Kind -> Vector2
getInitSpeed (BigPlayer _)   = (0, 0)
getInitSpeed (SmallPlayer _) = (0, 0)
getInitSpeed Gumba           = (-1 * tileSize, 0)
getInitSpeed Turtle          = (-1 * tileSize, 0)
getInitSpeed Mushroom        = (-1 * tileSize, 0)
getInitSpeed HpMushroom      = (-1 * tileSize, 0)
getInitSpeed Star            = (-1 * tileSize, g)
getInitSpeed Shell           = (-2 * tileSize, 0)
getInitSpeed Flagpole        = (0, 0)

-- ------------------------ Game initialization ------------------------ --

-- | Init state of the game.
initGame :: [Level] -> Game
initGame levels = Game
    { gameLevels = levels
    , gameCurLevel = currLevel
    , gamePlayers = [initPlayer 0 (levelInitPoint currLevel)]
    , gameCoins = 0
    , gameLvlNum = lvlNum
    , gameNextLvlNum = Just (-1)
    , pressedKeys = S.empty
    }
  where
    lvlNum = 0
    currLevel = (levels !! lvlNum)

-- | Initial state of the player.
initPlayer :: Int -> Coord -> Player
initPlayer n coord = createPlayer (SmallPlayer n) coord 3 False

-- | Create the player.
createPlayer :: Kind -> Coord -> Integer -> Bool -> Player
createPlayer kind coord hp isDead = Player
  { playerObj =
      MovingObject kind (mapCoordToPos coord) (0.0, 0.0) (0.0, 0.0) 0 5
  , playerHp = hp
  , playerIsDead = isDead
  }

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
  = l { levelMap = updateElemInMatrix (levelMap l) pos tile } : ls
updateLvlMap (l:ls) n pos tile = l : updateLvlMap ls (n - 1) pos tile

-- | Update the tile in the level.
updateElemInMatrix :: [[a]] -> Coord -> a -> [[a]]
updateElemInMatrix [] _ _ = []
updateElemInMatrix (l:ls) (pos_x, 0) tile
  = updateElemInList l pos_x tile : ls
updateElemInMatrix (l:ls) (pos_x, pos_y) tile
  = l : updateElemInMatrix ls (pos_x, pos_y - 1) tile

-- | Update element in list.
updateElemInList :: [a] -> Integer -> a -> [a]
updateElemInList [] _ _ = []
updateElemInList (_:xs) 0 newElem = newElem : xs
updateElemInList (x:xs) n newElem = x
  : updateElemInList xs (n - 1) newElem

-- | Remove the item from the list.
removeFromList :: Int -> [a] -> [a]
removeFromList _ [] = []
removeFromList i (h:t)
  | i == 0 = t
  | otherwise = h : removeFromList (i - 1) t

-- | Translate position to the coords for the map.
mapPosToCoord :: Position -> Coord
mapPosToCoord (x, y) = (div' x tileSize, div' y tileSize)

-- | Translate position to the coords for the map.
mapCoordToPos :: Coord -> Position
mapCoordToPos (x, y)
  = (fromIntegral x * tileSize, fromIntegral y * tileSize)

-- ------------------------ General functions ------------------------ --

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
applyToParts funForFold base posFun lvl (size_x, size_y) (pos_x, pos_y)
  = foldr funForFold base
  (map (\(x, y) -> posFun lvl (pos_x + offset x, pos_y + offset y))
    [(a, b) | a <- [0..count_x - 1], b <- [0..count_y - 1]])
  where
    (count_x, count_y) = (div' size_x minObjSize, div' size_y minObjSize)
    offset n = fromIntegral n * minObjSize

-- | Returns the scale for this screen and level.
getGameScale :: ScreenSize -> LevelMap -> Float
getGameScale (_, res_y) lvlMap = gameScaleFactor * (fromIntegral res_y) / getMapHeight lvlMap

-- | Based on tile count of stored map calculate map size
getMapHeight :: LevelMap -> Float
getMapHeight lvlMap = fromIntegral (length lvlMap) * tileSize

-- | Get the center between players.
centerOfScreen :: ScreenSize -> LevelMap -> [Player] -> Float
centerOfScreen _ _ [] = 100
centerOfScreen screenSize@(width, _) lvlMap players
  | res < offsetL = offsetL
  | res > offsetR = res
  | otherwise = res
  where
    gameScale = getGameScale screenSize lvlMap
    offset = (fromIntegral width) / (2 * gameScale) - tileSize/2
    size = tileSize * (fromIntegral $ length (lvlMap !! 0))
    res = mean $ foldr (minMax . getPos . playerObj) (pos1_x, pos1_x) (tail players)
    (offsetL, offsetR) = (offset, size - offset*2)
    (pos1_x, _) = (getPos . playerObj . head) players
    getPos (MovingObject _ pos _ _ _ _) = pos
    mean (lim_l, lim_r) = (lim_l + lim_r) / 2
    minMax (pos_x, _) lim@(lim_l, lim_r)
      | pos_x < lim_l = (pos_x, lim_r)
      | pos_x > lim_r = (lim_l, pos_x)
      | otherwise = lim
