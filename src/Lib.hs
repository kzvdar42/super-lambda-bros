{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Lib where

-- Graphics
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as G
-- Float mod
import Data.Fixed (div')
-- ------------------------ Game types ------------------------ --

-- | Tile of level.
data Tile = Ground | Brick | BonusBlockActive | BonusBlockEmpty | Empty

-- | Level of the game.
type Level = [[Tile]]

-- | State of the game (HP levelNumber nextLevel).
data GameState = GameState
  { gameStateHp          :: Int
  , gameStateLvlNum      :: Int
  , gameStateNextLvlNum  :: Maybe Int
  }

-- | Game.       Levels  Player       Level Objects  State
data Game = Game [Level] MovingObject [MovingObject] GameState

-- Objects.
type Vector2 = (Float, Float)
type Position = Vector2
type Velocity = Vector2
type Acceleration = Vector2
type Size = Vector2


-- | Data type for the objects o the level.
data MovingObject
  = MovingObject Kind Position Velocity Acceleration

-- | Kind of the MovingObject.
data Kind
  -- Player.
  = BigPlayer
  | SmallPlayer
  -- Enemies.
  | Gumba
  | Turtle
  -- Items.
  | Mushroom
  | Star
  | Shell

-- | Types of collisions.
data CollisionType = Delete | Spawn Kind Position | Change Tile

-- ------------------------ Game scale ------------------------ --

-- | Size of the tiles.
tileSize::Float
tileSize = 1

-- | Size of the minimum MovingObject.
-- Size of the others should be a scalar multiplication of this.
minObjSize::Float
minObjSize = 0.8*tileSize

-- | Size of the text.
textScale::Float
textScale = 0.01

-- | Game scale.
gameScale::Float
gameScale = 30

-- ------------------------ Game constants ------------------------ --

-- | Gravity of the world.
g :: Float
g = 0.01 * tileSize

-- | Friction rate of the tiles.
tileFrictionRate :: Tile -> Float
tileFrictionRate Empty = 0
tileFrictionRate _ = 0.05

-- | Step of Player (speed).
step :: Vector2
step = (1 * tileSize, 8 * tileSize)

-- | Thresh of collision distance.
-- If collisions doesn't work play with it.
thresh :: Float
thresh = 0.1 * tileSize

-- | Can Objects move through this tile?
canPass :: Tile -> Bool
canPass Empty = True
canPass _ = False

-- | Type of collision with player.
typeOfCollision :: Tile -> [CollisionType]
typeOfCollision Brick = [Delete]
typeOfCollision BonusBlockActive
  = [Spawn Mushroom (0, 1 * tileSize), Change BonusBlockEmpty]
typeOfCollision _ = []

-- | Get size of `MovingObject of given Kind.
getSize :: Kind -> Size
getSize BigPlayer = (minObjSize, minObjSize*2)
getSize SmallPlayer = (minObjSize, minObjSize)
getSize Gumba = (minObjSize, minObjSize)
getSize Turtle = (minObjSize, minObjSize*2)
getSize Mushroom = (minObjSize, minObjSize)
getSize Star = (minObjSize, minObjSize)
getSize Shell = (minObjSize, minObjSize)

-- ------------------------ Sample game ------------------------ --

-- | Function for ease of making big amounts of tiles.
makeTiles :: Tile -> [Tile]
makeTiles tile = [tile] ++ makeTiles tile

-- | Sample level for the game.
sampleLevel :: Level
sampleLevel =
  [ Ground : take 15 (makeTiles Ground) ++ [Ground] -- Bottom 0
  , Brick  : take 4 (makeTiles Empty) ++ [Brick]
    ++ take 10 (makeTiles Empty) ++ [Brick]
  , Brick  : take 15 (makeTiles Empty) ++ [Brick]
  , Brick  : take 3  (makeTiles Empty) ++ take 2  (makeTiles Brick)
    ++ [BonusBlockActive] ++ take 9  (makeTiles Brick) ++ [Brick]
  , Brick  : take 15 (makeTiles Empty) ++ [Brick]
  , Brick  : take 3 (makeTiles Empty) ++ [Brick]
    ++ take 11 (makeTiles Empty) ++ [Brick]
  , Brick  : take 15 (makeTiles Brick) ++ [Brick] -- Sky 6
  ]

-- | Init state of the game.
initState :: GameState
initState =  GameState
  { gameStateHp         = 3
  , gameStateLvlNum     = 0
  , gameStateNextLvlNum = Nothing
  }

-- | Initial state of the player.
initPlayer :: MovingObject
initPlayer = MovingObject SmallPlayer (1.0 * tileSize, 1.0 * tileSize) (0.0, 0.0) (0.0, 0.0)

-- | Initial amount of enemies.
initObjects :: [MovingObject]
initObjects =
  [ MovingObject Gumba (3.0 * tileSize, 1.0 * tileSize) (-1.0 * tileSize, 0.0) (0.0, 0.0)
  , MovingObject Turtle (4.0 * tileSize, 1.0 * tileSize) (1.0 * tileSize, 0.0) (0.0, 0.0)
  , MovingObject Mushroom (7.0 * tileSize, 4.0 * tileSize) (2.0 * tileSize, 0.0) (0.0, 0.0)
  ]

-- ------------------------ Working with map ------------------------ --

-- | Safely take the tile with given indexes from the level.
takeTileFromLvl :: Level -> Integer -> Integer -> Maybe Tile
takeTileFromLvl [] _ _ = Nothing
takeTileFromLvl (l:_) pos_x 0 = takeTileFromList l pos_x
takeTileFromLvl (_:ls) pos_x pos_y = takeTileFromLvl ls pos_x (pos_y - 1)

-- | Safely take tile from the tile row.
takeTileFromList :: [Tile] -> Integer -> Maybe Tile
takeTileFromList [] _ = Nothing
takeTileFromList (l:_) 0 = Just l
takeTileFromList (_:ls) n = takeTileFromList ls (n - 1)

-- | Update the level in the list.
updateLvls :: [Level] -> Int -> (Integer, Integer) -> Tile -> [Level]
updateLvls [] _ _ _ = []
updateLvls (l:ls) 0 pos tile = updateLvl l pos tile : ls
updateLvls (l:ls) n pos tile = l : updateLvls ls (n - 1) pos tile

-- | Update the tile in the level.
updateLvl :: Level -> (Integer, Integer) -> Tile -> Level
updateLvl [] _ _ = []
updateLvl (l:ls) (pos_x, 0) tile = updateRow l pos_x tile : ls
updateLvl (l:ls) (pos_x, pos_y) tile = l : updateLvl ls (pos_x, pos_y - 1) tile

-- | Update the tile in the row.
updateRow :: [Tile] -> Integer -> Tile -> [Tile]
updateRow [] _ _ = []
updateRow (_:ls) 0 tile = tile : ls
updateRow (l:ls) n tile = l : updateRow ls (n - 1) tile

mapPosToCoord :: Vector2 -> (Integer, Integer)
mapPosToCoord (x, y) = (div' x tileSize, div' y tileSize)

-- ------------------------ Physics ------------------------ --

-- | Check if the player's head collides with some block.
-- And if is, run the `performCollisions`.
checkCollision :: Game -> Game
checkCollision game@(Game levels player _ state) =
  case takeTileFromLvl level x y of
    Nothing -> game
    Just tile -> performCollisions (map (\c -> (c, (x, y))) (typeOfCollision tile)) game
  where
    (x, y) = mapPosToCoord (pos_x, pos_y + (snd (getSize kind)) + thresh)
    (pos_x, pos_y) = pos
    (MovingObject kind pos _ _ ) = player
    level = levels !! gameStateLvlNum state -- TODO: make this is a safe way.

-- | Perform the collisions.
-- Right now the implementation is fixed to the position of the player.
performCollisions :: [(CollisionType, (Integer, Integer))] -> Game -> Game
performCollisions [] game = game
performCollisions (c:cs) (Game levels player objects state) =
  case c of
    (Delete, tile_pos) -> performCollisions cs
      (Game (updateLvls levels levelNum tile_pos Empty) 
      (MovingObject objKind pos (vel_x, -vel_y) (accel_x, g)) objects state)
    (Spawn kind (off_x, off_y), (tile_x, tile_y)) -> performCollisions cs
      (Game levels player
      ((MovingObject kind (fromIntegral tile_x + off_x, fromIntegral tile_y + off_y)
      (1.0 * tileSize, 0.0) (0.0, 0.0)) : objects) state)
    (Change tile, tile_pos) -> performCollisions cs
      (Game (updateLvls levels levelNum tile_pos tile) player objects state)
  where
    (MovingObject objKind pos (vel_x, vel_y) (accel_x, _)) = player
    levelNum = gameStateLvlNum state

-- | Apply gravity to the `MovingObject`.
applyGravity :: MovingObject -> MovingObject
applyGravity (MovingObject kind pos vel (accel_x, accel_y))
  = MovingObject kind pos vel (accel_x, accel_y - g)

-- | Apply friction to the `MovingObject`.
applyFriction :: Level -> MovingObject -> MovingObject
applyFriction level object@(MovingObject kind pos (vel_x, vel_y) accel) =
  case takeTileFromLvl level (floor pos_x) (floor (pos_y - 0.01)) of
    Nothing -> object
    Just tile ->
      MovingObject kind pos (vel_x - vel_x*tileFrictionRate tile, vel_y) accel
  where
    (pos_x, pos_y) = pos

-- | Jump to the stars!
makeJump :: Level -> MovingObject -> Vector2 -> MovingObject
makeJump level player@(MovingObject kind pos (vel_x, vel_y) accel) (off_x, off_y) =
  case takeTileFromLvl level (floor pos_x) (floor (pos_y - 0.01))  of
  Nothing -> player
  Just tile ->
    if not (canPass tile) 
      then MovingObject kind pos new_vel accel
      else player
  where
    (pos_x, pos_y) = pos
    new_vel = (vel_x + off_x, vel_y + off_y)

-- | Updating the speed of Object due to user input.
changeSpeed :: MovingObject -> Vector2 -> MovingObject
changeSpeed (MovingObject kind pos (vel_x, vel_y) accel) (off_x, off_y)
  = MovingObject kind pos (vel_x + off_x, vel_y + off_y) accel

-- | Move Object due to it's velocity and acceleration.
move :: Float -> MovingObject -> MovingObject
move dt (MovingObject kind  (pos_x, pos_y) (vel_x, vel_y) accel@(accel_x, accel_y)) =
  MovingObject kind (new_x, new_y) (vel_x + accel_x, vel_y + accel_y) accel
  where
    new_x = pos_x + vel_x * dt + accel_x * dt ** 2 / 2
    new_y = pos_y + vel_y * dt + accel_y * dt ** 2 / 2

-- ------------------------ Game Engine ------------------------ --

-- | Init state of the game.
initGame :: Game
initGame = Game [sampleLevel] initPlayer initObjects initState

-- | Physics of the game.
updateGame :: Float -> Game -> Game
updateGame dt (Game levels player objects state) =
  case gameStateNextLvlNum state of
    Nothing -> checkCollision (Game levels upd_player upd_objects state)
    Just nextLevel -> Game levels upd_player upd_objects
      (state {gameStateLvlNum = nextLevel, gameStateNextLvlNum = Nothing})
  where
    level = levels !! gameStateLvlNum state -- TODO: make this is a safe way.
    upd_player
      = (tryMove dt level . applyFriction level . applyGravity) player
    upd_objects = map (applyGravity . tryMove dt level) objects

-- | Try to move thethe `MovingObject` by given offset.
tryMove :: Float -> Level -> MovingObject -> MovingObject
tryMove dt level object@(MovingObject kind old_pos@(old_x, old_y) _ _)
  | canMoveAtThisLvl (new_x, new_y) = new_obj
  | canMoveAtThisLvl (old_x, new_y) && (isPlayer kind)
  = move dt (MovingObject kind old_pos (0.0, vel_y) (0.0, accel_y))
  | canMoveAtThisLvl (old_x, new_y)
  = move dt (MovingObject kind old_pos (-vel_x, vel_y) (-accel_x, accel_y))
  | canMoveAtThisLvl (new_x, old_y)
  = move dt (MovingObject kind old_pos (vel_x, 0.0) (accel_x, 0.0))
  | isPlayer kind
  = MovingObject kind old_pos (0.0, 0.0) (0.0, 0.0)
  | otherwise
  = MovingObject kind old_pos (-vel_x, vel_y) (-accel_x, accel_y)
  where
    canMoveAtThisLvl = canThisBigObjectMove level (getSize kind)
    new_obj@(MovingObject
      _ (new_x, new_y) (vel_x, vel_y) (accel_x, accel_y)) = move dt object

    isPlayer BigPlayer = True
    isPlayer SmallPlayer = True
    isPlayer _ = False

-- | Checks if the complex `MovingObject` can move at given position.
canThisBigObjectMove :: Level -> Size -> Position -> Bool
canThisBigObjectMove lvl (size_x, size_y) (pos_x, pos_y)
  = foldr1 (&&)
  (map (\(x, y) -> canMove lvl (pos_x + x*minObjSize, pos_y + y*minObjSize))
  [(a, b)|a <- [0..size_x - 1], b <- [0..size_y - 1]])

-- | Checks if the simple `MovingObject` can move at this position.
canMove :: Level -> Position -> Bool
canMove lvl pos@(pos_x, pos_y) =
  case (left_bot, left_top, right_bot, right_top) of
  (Just t1, Just t2, Just t3, Just t4) ->
    canPass t1 && canPass t2 && canPass t3 && canPass t4
  _ -> False
  where
    left_bot = takeTileFromLvl lvl x y
    left_top = takeTileFromLvl lvl x (y_r)
    right_bot = takeTileFromLvl lvl (x_r) y
    right_top = takeTileFromLvl lvl (x_r) (y_r)
    (x, y) = mapPosToCoord pos
    (x_r, y_r) = mapPosToCoord (pos_x + minObjSize, pos_y + minObjSize)

-- | Update Player speed due to user input.
handleGame :: G.Event -> Game -> Game
handleGame (G.EventKey key keyState _ _) (Game levels player objects state)
  | G.SpecialKey G.KeyUp <- key
  , G.Down  <- keyState
  = Game levels (makeJump level player (0.0, snd step)) objects  state
    where
      level = levels !! gameStateLvlNum state -- TODO: make this safe.
handleGame (G.EventKey key keyState _ _) (Game levels player objects state)
  | G.SpecialKey G.KeyLeft <- key
  , G.Down    <- keyState
  = Game levels (changeSpeed player (- fst step, 0.0)) objects state
handleGame (G.EventKey key keyState _ _) (Game levels player objects state)
  | G.SpecialKey G.KeyRight <- key
  , G.Down    <- keyState
  = Game levels (changeSpeed player (fst step, 0.0)) objects state
handleGame _ game  = game

-- ------------------------ Drawing the game ------------------------ --

-- | Draw the game.
drawGame :: Game -> Picture
drawGame (Game levels player objects state) =
  let
    level = levels !! (gameStateLvlNum state) -- TODO: do this in a safe way
  in
    translate (gameScale * tileSize / 2) (gameScale * tileSize / 2) (scale gameScale gameScale (drawLevel level))
    <> scale gameScale gameScale (pictures (map (drawObject) objects))
    <> scale gameScale gameScale (drawObject player)

-- | Draw the level.
drawLevel :: Level -> Picture
drawLevel [] = blank
drawLevel (l:ls)
  = drawLine l
  <> (translate 0 (tileSize) (drawLevel ls))

-- | Draw the line of the level.
drawLine :: [Tile] -> Picture
drawLine [] = blank
drawLine (tile:tiles)
  = drawTile tile
  <> translate tileSize 0 (drawLine tiles)


-- | Draw one tile.
drawTile :: Tile -> Picture
drawTile Ground = color orange (rectangleSolid tileSize tileSize)
drawTile Brick = color red (rectangleSolid tileSize tileSize)
drawTile BonusBlockActive = color yellow (rectangleSolid tileSize tileSize)
drawTile BonusBlockEmpty = color orange (rectangleSolid tileSize tileSize)
drawTile Empty = color white (rectangleSolid tileSize tileSize)

-- | Draw object.
drawObject :: MovingObject -> Picture
drawObject (MovingObject objType (pos_x, pos_y) _ _)
  = translate (pos_x + size_x/2) (pos_y + size_y/2) (drawKind objType)
  where
    (size_x, size_y) = getSize objType

-- | Draw the objectKing.
drawKind :: Kind -> Picture
drawKind BigPlayer 
  = scale tileSize tileSize 
  (color red (rectangleSolid (fst (getSize BigPlayer)) (snd (getSize BigPlayer))))
drawKind SmallPlayer
  = scale tileSize tileSize
  (color red (rectangleSolid (fst (getSize SmallPlayer)) (snd (getSize SmallPlayer))))
drawKind Gumba
  = scale tileSize tileSize 
  (color blue (rectangleSolid (fst (getSize Gumba)) (snd (getSize Gumba))))
drawKind Turtle
  = scale tileSize tileSize 
  (color green (rectangleSolid (fst (getSize Turtle)) (snd (getSize Turtle))))
drawKind Mushroom
  = scale tileSize tileSize 
  (color blue (rectangleSolid (fst (getSize Mushroom)) (snd (getSize Mushroom))))
drawKind Star
  = scale tileSize tileSize 
  (color yellow(rectangleSolid (fst (getSize Star)) (snd (getSize Star))))
drawKind Shell
  = scale tileSize tileSize 
  (color green (rectangleSolid (fst (getSize Shell)) (snd (getSize Shell))))

