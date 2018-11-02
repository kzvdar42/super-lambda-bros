{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}


-- | Comment the line with module to run this file on `CodeWorld`.
module Lib where

import Graphics.Gloss
import qualified Graphics.Gloss.Interface.Pure.Game     as G
-- ------------------------ Game types ------------------------ --

-- | Tile of level.
data Tile = Ground | Brick | BonusBlockActive | BonusBlockEmpty | Empty
-- | Level of the game.
type Level = [[Tile]]
-- | State of the game (HP levelNumber nextLevel).
type GameState = (Int, Int, Maybe Int)
-- | Game.
data Game = Game [Level] MovingObject [MovingObject] GameState

-- Objects.
type Vector2 = (Float, Float)
type Position = Vector2
type Velocity = Vector2
type Acceleration = Vector2
-- | Data type for the objects o the level.
data MovingObject
  = MovingObject Position Velocity Acceleration Kind
-- | Kind of the MovingObject.
data Kind
  -- Player
  = Player
  -- Enemies
  | Gumba
  | Turtle
  -- Items
  | Mushroom
  | Star
  | Shell
-- | Types of collisions.
data CollisionType = Delete | Spawn Kind Position | Change Tile | Stay

-- ------------------------ Game constants ------------------------ --

tileSize::Float
tileSize = 1

-- | Gravity of the world.
g :: Float
g = 0.01*tileSize

-- | Friction rate of the tiles.
tileFrictionRate :: Tile -> Float
tileFrictionRate Empty = 0
tileFrictionRate _ = 0.05*tileSize

-- | Step of Player (speed).
step :: Vector2
step = (1*tileSize, 4*tileSize)

-- | Thresh of collision distance.
-- If collisions doesn't work play with it.
thresh :: Float
thresh = 0.2*tileSize

-- | Can Objects move through this tile?
canPass :: Tile -> Bool
canPass Empty = True
canPass _ = False

-- | Type of collision with player.
typeOfCollision :: Tile -> [CollisionType]
typeOfCollision Brick = [Delete]
typeOfCollision BonusBlockActive 
  = Spawn Mushroom (0, 1*tileSize) : [Change BonusBlockEmpty]
typeOfCollision _ = [Stay]


-- ------------------------ Sample game ------------------------ --

-- | Function for ease of making big amounts of tiles.
makeTiles :: Tile -> [Tile]
makeTiles tile = [tile] ++ makeTiles tile

-- | Sample level for the game.
sampleLevel :: Level
sampleLevel = [
  Ground : take 15 (makeTiles Ground)++ [Ground], -- Bottom 0
  Brick  : take 15 (makeTiles Empty) ++ [Brick],
  Brick  : take 15 (makeTiles Empty) ++ [Brick],
  -- Brick  : take 3  (makeTiles Empty) ++ take 2  (makeTiles Empty) 
  -- ++ [BonusBlockActive] ++ take 9  (makeTiles Brick) ++ [Brick],
  Brick  : take 15 (makeTiles Empty) ++ [Brick],  
  Brick  : take 15 (makeTiles Empty) ++ [Brick],
  Brick  : take 15 (makeTiles Empty) ++ [Brick],
  Brick  : take 15 (makeTiles Brick) ++ [Brick]   -- Sky 6
  ]

-- | Init state of the game.
initState :: GameState
initState = (3, 0, Nothing)

-- | Initial state of the player.
initPlayer :: MovingObject
initPlayer = MovingObject (1.0*tileSize, 1.0*tileSize) (0.0, 0.0) (0.0, 0.0) Player

-- | Initial amount of enemies.
initEnemies :: [MovingObject]
initEnemies = [
  MovingObject (3.0*tileSize, 1.0*tileSize) (-1.0*tileSize, 0.0) (0.0, 0.0) Gumba,
  MovingObject (4.0*tileSize, 1.0*tileSize) (1.0*tileSize, 0.0) (0.0, 0.0) Turtle,
  MovingObject (7.0*tileSize, 4.0*tileSize) (2.0*tileSize, 0.0) (0.0, 0.0) Mushroom
  ]

-- ------------------------ Working with map ------------------------ --

-- | Safely take the tile with given indexes from the level.
takeTileFromLevel :: Level -> Integer -> Integer -> Maybe Tile
takeTileFromLevel [] _ _ = Nothing
takeTileFromLevel (l:_) pos_x 0 = takeTileFromList l pos_x
takeTileFromLevel (_:ls) pos_x pos_y = takeTileFromLevel ls pos_x (pos_y-1)

-- | Safely take tile from the tile row.
takeTileFromList :: [Tile] -> Integer -> Maybe Tile
takeTileFromList [] _ = Nothing
takeTileFromList (l:_) 0 = Just l
takeTileFromList (_:ls) n = takeTileFromList ls (n-1)

-- | Update the level in the list.
updateLevels :: [Level] -> Int -> (Integer, Integer) -> Tile -> [Level]
updateLevels [] _ _ _ = []
updateLevels (l:ls) 0 pos tile = updateLevel l pos tile : ls
updateLevels (l:ls) n pos tile = l : updateLevels ls (n-1) pos tile

-- | Update the tile in the level.
updateLevel :: Level -> (Integer, Integer) -> Tile -> Level
updateLevel [] _ _ = [] 
updateLevel (l:ls) (pos_x, 0) tile = updateRow l pos_x tile : ls
updateLevel (l:ls) (pos_x, pos_y) tile = l : updateLevel ls (pos_x, pos_y-1) tile

-- | Update the tile in the row.
updateRow :: [Tile] -> Integer -> Tile -> [Tile]
updateRow [] _ _ = []
updateRow (_:ls) 0 tile = tile : ls
updateRow (l:ls) n tile = l : updateRow ls (n-1) tile

-- ------------------------ Physics ------------------------ --

-- | Check if there's some collisions.
-- And if is, run the `performCollisions`.
checkCollision :: Game -> Game
checkCollision game@(Game levels player _ state) =
  case takeTileFromLevel level x y of
    Nothing -> game
    Just tile -> performCollisions (typeOfCollision tile) game
  where
    (x, y) = (ceiling pos_x, ceiling (pos_y + thresh))
    (MovingObject (pos_x, pos_y) _ _ _) = player
    (_, levelNum, _) = state
    level = levels !!levelNum

-- | Perform the collisions.
-- Right now the implementation is fixed to the position of the player.
performCollisions :: [CollisionType] -> Game -> Game
performCollisions [] game = game
performCollisions (c:cs) game@(Game levels player objects state) = 
  case c of
    Delete -> performCollisions cs
      (Game (updateLevels levels levelNum (x, y) Empty) player objects state)
    Spawn kind (off_x, off_y) -> performCollisions cs
      (Game levels player
      ((MovingObject (fromIntegral x + off_x, fromIntegral y + off_y) 
      (1.0*tileSize, 0.0) (0.0, 0.0) kind) : objects) state)
    Change tile -> performCollisions cs
      (Game (updateLevels levels levelNum (x, y) tile) player objects state)
    Stay -> performCollisions cs game
  where
    (x, y) = (ceiling pos_x, ceiling (pos_y + thresh))
    (MovingObject pos _ _ _) = player
    (_, levelNum, _) = state
    (pos_x, pos_y) = pos

-- | Apply gravity to the `MovingObject`.
applyGravity :: MovingObject -> MovingObject
applyGravity (MovingObject pos vel accel objKind)
  = MovingObject pos vel (accel_x, accel_y - g) objKind
  where
    (accel_x, accel_y) = accel

-- | Apply friction to the `MovingObject`.
applyFriction :: Level -> MovingObject -> MovingObject
applyFriction level object@(MovingObject pos vel accel objKind) =
  case takeTileFromLevel level (floor pos_x) (floor (pos_y-0.01)) of
    Nothing -> object
    Just tile -> MovingObject pos (vel_x - vel_x*tileFrictionRate tile, vel_y)
      accel objKind
  where
    (pos_x, pos_y) = pos
    (vel_x, vel_y) = vel

-- | Jump to the stars!
makeJump :: Level -> MovingObject -> Vector2 -> MovingObject
makeJump level player@(MovingObject pos vel accel kind) (off_x, off_y) =
  case takeTileFromLevel level (floor pos_x) (floor (pos_y-0.01))  of
  Nothing -> player
  Just tile -> 
    if not (canPass tile) then MovingObject pos new_vel accel kind else player
  where
    (pos_x, pos_y) = pos
    (vel_x, vel_y) = vel
    new_vel = (vel_x + off_x, vel_y + off_y)

-- | Updating the speed of Object due to user input.
changeSpeed :: MovingObject -> Vector2 -> MovingObject
changeSpeed (MovingObject pos vel accel kind) (off_x, off_y)
  = MovingObject pos new_vel accel kind
  where
    (vel_x, vel_y) = vel
    new_vel = (vel_x + off_x, vel_y + off_y)

-- ------------------------ Game Engine ------------------------ --

-- | Init state of the game.
initGame :: Game
initGame = Game [sampleLevel] initPlayer initEnemies initState

-- | Physics of the game.
updateGame :: Float -> Game -> Game
updateGame dt (Game levels player objects state) = 
  case maybeNextLevel of
    Nothing -> checkCollision (Game levels upd_player upd_objects state)
    Just nextLevel -> Game levels upd_player upd_objects (hp, nextLevel, Nothing)
  where
    (hp, levelNum, maybeNextLevel) = state
    level = levels !! levelNum -- TODO: make this is a safe way
    upd_player
      = (applyFriction level . applyGravity . tryMove dt level) player
    upd_objects = map (applyGravity . tryMove dt level) objects

-- | Try to move thethe `MovingObject` by given offset.
tryMove :: Float -> Level -> MovingObject -> MovingObject
tryMove dt level object@(MovingObject old_pos _ _ objKind)
  | canMove level (new_x, new_y) = new_obj
  | canMove level (old_x, new_y)
  = move dt (MovingObject old_pos (0.0, vel_y) (0.0, accel_y) objKind)
  | canMove level (new_x, old_y)
  = move dt (MovingObject old_pos (vel_x, 0.0) (accel_x, 0.0) objKind)
  | isPlayer objKind
  = MovingObject old_pos (0.0, 0.0) (0.0, 0.0) objKind
  | otherwise
  = MovingObject old_pos (-vel_x, vel_y) (-accel_x, accel_y) objKind
  where
    new_obj@(MovingObject new_pos vel accel _) = move dt object
    isPlayer Player = True
    isPlayer _ = False
    (vel_x, vel_y) = vel
    (accel_x, accel_y) = accel
    (old_x, old_y) = old_pos
    (new_x, new_y) = new_pos

-- | Checks if the `MovingObject` can move at this position.
canMove :: Level -> Position -> Bool
canMove level pos =
  case (left_top, left_down, right_top, right_down) of
  (Just t1, Just t2, Just t3, Just t4) ->
    canPass t1 && canPass t2 && canPass t3 && canPass t4
  _ -> False
  where
    (pos_x, pos_y) = pos
    left_top = takeTileFromLevel level left top
    left_down = takeTileFromLevel level left down
    right_top = takeTileFromLevel level right top
    right_down = takeTileFromLevel level right down
    left = floor pos_x
    right = ceiling pos_x
    top = ceiling pos_y
    down = floor pos_y

-- | Move Object due to it's velocity and acceleration.
move :: Float -> MovingObject -> MovingObject
move dt (MovingObject pos vel accel kind) =
  MovingObject (new_x, new_y) (vel_x + accel_x, vel_y + accel_y) accel kind
  where
    (pos_x, pos_y) = pos
    (vel_x, vel_y) = vel
    (accel_x, accel_y) = accel
    new_x = pos_x + vel_x * dt + accel_x * dt * dt /2
    new_y = pos_y + vel_y * dt + accel_y * dt * dt /2

-- | Update Player speed due to user input.
-- handleGame :: Event -> Game -> Game
-- handleGame (KeyPress "Up") (Game levels player objects state)
--   = Game levels (makeJump level player (0.0, snd step)) objects  state
--   where
--     (_, levelNum, _) = state
--     level = levels !! levelNum
-- handleGame (KeyPress "Left") (Game levels player objects state)
--   = Game levels (changeSpeed player (-fst step, 0.0)) objects state
-- handleGame (KeyPress "Right") (Game levels player objects state)
--   = Game levels (changeSpeed player (fst step, 0.0)) objects  state
-- handleGame _ game  = game

handleGame :: G.Event -> Game -> Game
handleGame (G.EventKey key keyState _ _) (Game levels player objects state)
  | G.SpecialKey G.KeyUp <- key
  , G.Down  <- keyState
  = Game levels (makeJump level player (0.0, snd step)) objects  state
    where
      (_, levelNum, _) = state
      level = levels !! levelNum
handleGame (G.EventKey key keyState _ _) (Game levels player objects state)
  | G.SpecialKey G.KeyLeft <- key
  , G.Down    <- keyState
  = Game levels (changeSpeed player (-fst step, 0.0)) objects state
handleGame (G.EventKey key keyState _ _) (Game levels player objects state)
  | G.SpecialKey G.KeyRight <- key
  , G.Down    <- keyState
  = Game levels (changeSpeed player (fst step, 0.0)) objects state
handleGame _ game  = game

gameScale::Float
gameScale = 30

-- | Draw the gane.
drawGame :: Game -> Picture
drawGame (Game levels player objects state) =
  let 
    (_, levelNum, _) = state
    level = levels !! levelNum -- TODO: do this in a safe way
  in
    scale gameScale gameScale (drawLevel level)
    <> scale gameScale gameScale (pictures (map (drawObject) objects))
    <> scale gameScale gameScale (drawObject player)

-- ------------------------ Code World ------------------------ --

-- | Draw the level.
drawLevel :: Level -> Picture
drawLevel [] = blank
drawLevel (line:ls)
  = drawLine line
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
drawTile BonusBlockActive = text "?" <> color yellow (rectangleSolid tileSize tileSize)
drawTile BonusBlockEmpty = color orange (rectangleSolid tileSize tileSize)
drawTile Empty = color white (rectangleSolid tileSize tileSize)


textScale::Float
textScale = 0.01
-- | Draw the objectKing.
drawKind :: Kind -> Picture
drawKind Player = scale textScale textScale (text "P")
drawKind Gumba = scale textScale textScale (text "G")
drawKind Turtle = scale textScale textScale (text "T")
drawKind Mushroom = scale textScale textScale (text "M")
drawKind Star = scale textScale textScale (text "S")
drawKind Shell = scale textScale textScale (text "SH")

-- | Draw object.
drawObject :: MovingObject -> Picture
drawObject (MovingObject pos _ _ objType) 
  = translate pos_x pos_y (drawKind objType)
  where
    (pos_x, pos_y) = pos

