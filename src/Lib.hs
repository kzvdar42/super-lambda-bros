{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import CodeWorld

-- ------------------------ Game types ------------------------ --

-- | Tile of map.
data Tile = Ground | Brick | BonusBlock | Empty
-- | Map of the game.
type Map = [[Tile]]
-- | State of the game (HP mapNumber nextMap).
type GameState = (Int, Int, Maybe Int)
-- | Game.
data Game = Game [Map] MovingObject [MovingObject] GameState

-- Objects.
type Vector2 = (Double, Double)
type Position = Vector2
type Velocity = Vector2
type Acceleration = Vector2
-- | Data type for the objects o the map.
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

-- ------------------------ Game constants ------------------------ --

-- | Gravity of the world.
g :: Double
g = 0.01

-- | Friction rate of the tiles.
tileFrictionRate :: Tile -> Double
tileFrictionRate Ground = 0.05
tileFrictionRate Brick = 0.05
tileFrictionRate BonusBlock = 0.05
tileFrictionRate Empty = 0

-- | Step of Player (speed).
step :: Vector2
step = (1, 4)

-- | Can Objects move through this tile?
canPass :: Tile -> Bool
canPass Ground = False
canPass Brick = False
canPass BonusBlock = False
canPass Empty = True


-- ------------------------ Sample game ------------------------ --

makeTiles :: Tile -> [Tile]
makeTiles tile = [tile] ++ makeTiles tile

sampleMap :: Map
sampleMap = [
  Ground : take 15 (makeTiles Ground)++ [Ground], -- Bottom 0
  Brick  : take 15 (makeTiles Empty) ++ [Brick],
  Brick  : take 15 (makeTiles Empty) ++ [Brick],
  Brick  : take 3  (makeTiles Empty) ++ take 12  (makeTiles Brick)++ [Brick],
  Brick  : take 15 (makeTiles Empty) ++ [Brick],
  Brick  : take 15 (makeTiles Empty) ++ [Brick],
  Brick  : take 15 (makeTiles Brick) ++ [Brick]   -- Sky 6
  ]

initState :: GameState
initState = (3, 0, Nothing)

initPlayer :: MovingObject
initPlayer = MovingObject (1.0, 1.0) (0.0, 0.0) (0.0, 0.0) Player

initEnemies :: [MovingObject]
initEnemies = [
  MovingObject (3.0, 1.0) (-1.0, 0.0) (0.0, 0.0) Gumba,
  MovingObject (4.0, 1.0) (1.0, 0.0) (0.0, 0.0) Turtle,
  MovingObject (7.0, 4.0) (2.0, 0.0) (0.0, 0.0) Mushroom
  ]
-- ------------------------ Game Engine ------------------------ --

-- | Init state of the game.
initGame :: Game
initGame = Game [sampleMap] initPlayer initEnemies initState

-- | Physics of the game.
updateGame :: Double -> Game -> Game
updateGame dt (Game maps player objects state) = 
  case maybeNextLevel of
    Nothing -> Game maps upd_player upd_objects state
    Just nextLevel -> Game maps upd_player upd_objects (hp, nextLevel, Nothing)
  where
    (hp, mapNum, maybeNextLevel) = state
    currMap = maps !! mapNum -- TODO: make this is a safe way
    upd_player
      = (applyFriction currMap . applyGravity . tryMove dt currMap) player
    upd_objects = map (applyGravity . tryMove dt currMap) objects


-- | Apply gravity to the `MovingObject`.
applyGravity :: MovingObject -> MovingObject
applyGravity (MovingObject pos vel accel objKind)
  = MovingObject pos vel (accel_x, accel_y - g) objKind
  where
    (accel_x, accel_y) = accel

-- | Apply friction to the `MovingObject`.
applyFriction :: Map -> MovingObject -> MovingObject
applyFriction currMap object@(MovingObject pos vel accel objKind) =
  case takeTileFromMap currMap (floor pos_x) (floor (pos_y-0.01)) of
    Nothing -> object
    Just tile -> MovingObject pos (vel_x - vel_x*tileFrictionRate tile, vel_y)
      accel objKind
  where
    (pos_x, pos_y) = pos
    (vel_x, vel_y) = vel

-- | Try to move thethe `MovingObject` by given offset.
tryMove :: Double -> Map -> MovingObject -> MovingObject
tryMove dt currMap object@(MovingObject old_pos _ _ objKind)
  | canMove currMap (new_x, new_y) = new_obj
  | canMove currMap (old_x, new_y)
  = move dt (MovingObject old_pos (0.0, vel_y) (0.0, accel_y) objKind)
  | canMove currMap (new_x, old_y)
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
canMove :: Map -> Position -> Bool
canMove currMap pos =
  case (left_top, left_down, right_top, right_down) of
  (Just t1, Just t2, Just t3, Just t4) ->
    canPass t1 && canPass t2 && canPass t3 && canPass t4
  _ -> False
  where
    (pos_x, pos_y) = pos
    left_top = takeTileFromMap currMap left top
    left_down = takeTileFromMap currMap left down
    right_top = takeTileFromMap currMap right top
    right_down = takeTileFromMap currMap right down
    left = floor pos_x
    right = ceiling pos_x
    top = ceiling pos_y
    down = floor pos_y

-- | Take the tile with given indexes from the level map.
takeTileFromMap :: Map -> Integer -> Integer -> Maybe Tile
takeTileFromMap [] _ _ = Nothing
takeTileFromMap (l:_) pos_x 0 = takeTileFromList l pos_x
takeTileFromMap (_:ls) pos_x pos_y = takeTileFromMap ls pos_x (pos_y-1)

takeTileFromList :: [Tile] -> Integer -> Maybe Tile
takeTileFromList [] _ = Nothing
takeTileFromList (l:_) 0 = Just l
takeTileFromList (_:ls) n = takeTileFromList ls (n-1)

-- | Move Object due to it's velocity and acceleration.
move :: Double -> MovingObject -> MovingObject
move dt (MovingObject pos vel accel kind) =
  MovingObject (new_x, new_y) (vel_x + accel_x, vel_y + accel_y) accel kind
  where
    (pos_x, pos_y) = pos
    (vel_x, vel_y) = vel
    (accel_x, accel_y) = accel
    new_x = pos_x + vel_x * dt + accel_x * dt * dt /2
    new_y = pos_y + vel_y * dt + accel_y * dt * dt /2

-- | Update Player speed due to user input.
handleGame :: Event -> Game -> Game
handleGame (KeyPress "Up") (Game maps player objects state)
  = Game maps (makeJump currMap player (0.0, snd step)) objects  state
  where
    (_, mapNum, _) = state
    currMap = maps !!mapNum
handleGame (KeyPress "Left") (Game maps player objects state)
  = Game maps (changeSpeed player (-fst step, 0.0)) objects state
handleGame (KeyPress "Right") (Game maps player objects state)
  = Game maps (changeSpeed player (fst step, 0.0)) objects  state
handleGame _ game  = game

makeJump :: Map -> MovingObject -> Vector2 -> MovingObject
makeJump level player@(MovingObject pos vel accel kind) (off_x, off_y) =
  case takeTileFromMap level (floor pos_x) (floor (pos_y-0.01))  of
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

-- | Draw the gane.
drawGame :: Game -> Picture
drawGame (Game maps player objects state) =
  let
    (_, mapNum, _) = state
    currMap = maps !! mapNum -- TODO: do this in a safe way
  in
  drawObject player
  <> pictures (map (drawObject) objects)
  <> drawLevel currMap

-- ------------------------ Code World ------------------------ --

-- | Draw the level.
drawLevel :: Map -> Picture
drawLevel [] = blank
drawLevel (line:ls)
  = drawLine line
  <> (translated 0 (1) (drawLevel ls))

-- | Draw the line of the level.
drawLine :: [Tile] -> Picture
drawLine [] = blank
drawLine (tile:tiles)
  = drawTile tile
  <> translated 1 0 (drawLine tiles)

-- | Draw one tile.
drawTile :: Tile -> Picture
drawTile Ground = colored brown (solidRectangle 1 1)
drawTile Brick = colored red (solidRectangle 1 1)
drawTile BonusBlock = colored yellow (solidRectangle 1 1)
drawTile Empty = colored white (solidRectangle 1 1)

-- | Draw the objectKing.
drawKind :: Kind -> Picture
drawKind Player = lettering "Ĝ"
drawKind Gumba = lettering "??"
drawKind Turtle = lettering "??"
drawKind Mushroom = lettering "??"
drawKind Star = lettering "?"
drawKind Shell = lettering "??"

-- | Draw object.
drawObject :: MovingObject -> Picture
drawObject (MovingObject pos _ _ objType) 
  = translated pos_x pos_y (drawKind objType)
  where
    (pos_x, pos_y) = pos

-- | Starts the sample game.
runGame :: IO()
runGame = interactionOf initGame updateGame handleGame drawGame