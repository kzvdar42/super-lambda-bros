{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

-- Map
data Tile = Ground | Brick | BonusBlock | Empty
type Map = [[Tile]]
-- GameState = HP mapNumber Maybe nextMap
type GameState = (Int, Int, Maybe Int)
data Game = Game [Map] MovingObject [MovingObject] GameState

-- Objects
type Vector2 = (Double, Double)
type Position = Vector2
type Velocity = Vector2
type Acceleration = Vector2
data PlayerKind = Small | Big | Dead
data EnemyKind = Gumba | Turtle
data ItemKind = Mushroom | Star | Shell

data MovingObject
  = Player Position Velocity Acceleration PlayerKind
  | Enemy Position Velocity Acceleration EnemyKind
  | Item Position Velocity Acceleration ItemKind

-- ------------------------ Sample game ------------------------ --
makeTiles :: Tile -> [Tile]
makeTiles tile = [tile] ++ makeTiles tile

sampleMap :: Map
sampleMap = [
  take 30 (makeTiles Brick),
  take 30 (makeTiles Empty),
  take 30 (makeTiles Empty),
  take 30 (makeTiles Empty),
  take 30 (makeTiles Ground)
  ]

initState :: GameState
initState = (3, 0, Nothing)

initPlayer :: MovingObject
initPlayer = Player (0.0, 1.0) (0.0, 0.0) (0.0, 0.0) Small

initEnemies :: [MovingObject]
initEnemies = [
  Enemy (3.0, 1.0) (-1.0, 0.0) (0.0, 0.0) Gumba,
  Enemy (4.0, 1.0) (1.0, 0.0) (0.0, 0.0) Turtle,
  Item (7.0, 4.0) (-2.0, 0.0) (0.0, 0.0) Mushroom
  ]
-- ------------------------ Game Engine ------------------------ --

initGame :: Game
initGame = Game [sampleMap] initPlayer initEnemies initState

-- | Physics of the game.
updateGame :: Double -> Game -> Game
updateGame dt (Game maps player objects state) = Game maps upd_player upd_objects state
  where
    upd_player = move dt player
    upd_objects = map (move dt) objects

-- | Move Object due to it's velocity and acceleration.
move :: Double -> MovingObject -> MovingObject
move dt (Player pos vel accel kind) = Player (changePos dt pos vel accel) vel accel kind
move dt (Enemy pos vel accel kind)  = Enemy (changePos dt pos vel accel) vel accel kind
move dt (Item pos vel accel kind)   = Item (changePos dt pos vel accel) vel accel kind

-- | Change the position due to velocity and acceleration.
changePos :: Double -> Position -> Velocity -> Acceleration -> Position
changePos dt (pos_x, pos_y) (vel_x, vel_y) (accel_x, accel_y) = (new_x, new_y)
  where
    new_x = pos_x + vel_x* dt + accel_x*dt^2/2
    new_y = pos_y + vel_y* dt + accel_y*dt^2/2


-- | Step of Player (acceleration).
step :: Double
step = 9.3

-- | Update Player acceleration.
handleGame :: Event -> Game -> Game
handleGame (KeyPress "Up") (Game maps player objects state)    = Game maps (accel player (0.0, step)) objects  state
handleGame (KeyPress "Down") (Game maps player objects state)  = Game maps (accel player (0.0, -step)) objects state
handleGame (KeyPress "Left") (Game maps player objects state)  = Game maps (accel player (-step, 0.0)) objects state
handleGame (KeyPress "Right") (Game maps player objects state) = Game maps (accel player (step, 0.0)) objects  state
handleGame _ game  = game

-- | Updating the acceleration of Object. 
-- TODO: move all object types.
accel :: MovingObject -> Vector2 -> MovingObject
accel (Player pos vel accel kind) (off_x, off_y) 
  = Player pos vel new_accel kind
  where
    (accel_x, accel_y) = accel
    new_accel = (accel_x + off_x, accel_y + off_y)


drawGame :: Game -> Picture
drawGame (Game maps player objects state) =
  let (hp, mapNum, _) = state
  in
  drawObject player
  <> pictures (map (drawObject) objects)
  <> (drawWorld (maps !! mapNum))
      
-- ------------------------ Code World ------------------------ --

drawWorld :: Map -> Picture
drawWorld [] = blank
drawWorld (line:ls)
  = drawLine line
  <> (translated 0 (1) (drawWorld ls))

drawLine :: [Tile] -> Picture
drawLine [] = blank
drawLine (tile:tiles)
  = drawTile tile
  <> translated 1 0 (drawLine tiles)

drawTile :: Tile -> Picture
drawTile Ground = colored brown (solidRectangle 1 1)
drawTile Brick = colored red (solidRectangle 1 1)
drawTile BonusBlock = colored yellow (solidRectangle 1 1)
drawTile Empty = colored white (solidRectangle 1 1)

drawObject :: MovingObject -> Picture
drawObject (Player pos _ _ _) = translated pos_x pos_y (colored pink (solidRectangle 1 1))
 where
   (pos_x, pos_y) = pos
drawObject (Enemy  pos _ _ Gumba) = translated pos_x pos_y (colored brown (solidRectangle 1 1))
 where
   (pos_x, pos_y) = pos
drawObject (Enemy  pos _ _ Turtle) = translated pos_x pos_y (colored green (solidRectangle 1 1))
 where
   (pos_x, pos_y) = pos
drawObject (Item  pos _ _ Mushroom) = translated pos_x pos_y (colored brown (solidRectangle 1 1))
 where
   (pos_x, pos_y) = pos
drawObject (Item  pos _ _ Star) = translated pos_x pos_y (colored yellow (solidRectangle 1 1))
 where
   (pos_x, pos_y) = pos
drawObject (Item  pos _ _ Shell) = translated pos_x pos_y (colored orange (solidRectangle 1 1))
 where
   (pos_x, pos_y) = pos

main :: IO()
main = interactionOf initGame updateGame handleGame drawGame


