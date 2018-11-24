{-# OPTIONS_GHC -Wall #-}

module Update where

import qualified Data.Set as S

import Lib
import Data.Fixed (mod')
-- | Check if the player's head collides with some block.
-- And if is, run the `performCollisions`.
checkCollision :: Game -> Game
checkCollision game@(Game levels player _ state) =
  case takeElemFromMatrix level (x_close, y) of
    Nothing -> case takeElemFromMatrix level (x_far, y) of
      Nothing -> game
      Just tile -> performCollisions (map (\c -> (c, (x_far, y))) (typeOfCollision tile)) game
    Just tile -> performCollisions (map (\c -> (c, (x_close, y))) (typeOfCollision tile)) game
  where
    level = levels !! gameStateLvlNum state -- TODO: do this is a safe way.
    (MovingObject kind (pos_x, pos_y) _ _ _ _) = player
    (x, y) = mapPosToCoord (pos_x, pos_y + (snd (getSize kind)) + (tileSize - minObjSize) - thresh)  -- TODO: change when will generalize minObjSize
    (x_r, _) = mapPosToCoord (pos_x + (fst (getSize kind)), pos_y)
    (x_close, x_far) =
      if pos_x - (fromIntegral x) * tileSize < (fromIntegral x_r) * tileSize - pos_x
        then (x, x_r)
        else (x_r, x)

-- | Perform the collisions.
-- Right now the implementation is fixed to the position of the player.
performCollisions :: [(CollisionType, Coord)] -> Game -> Game
performCollisions [] game = game
performCollisions (c:cs) (Game lvls player objects state) =
  performCollisions cs $ case c of
    (Delete, tile_pos) ->
      (Game (updateLvls lvls levelNum tile_pos Empty) player objects state)
    (Spawn objKind (off_x, off_y), (tile_x, tile_y)) ->
      (Game lvls player
        ((MovingObject objKind (mapCoordToPos (tile_x + off_x, tile_y + off_y))
          (1.0 * tileSize, 0.0) (0.0, 0.0) animC animD) : objects) state)
    (Change tile, tile_pos) ->
      (Game (updateLvls lvls levelNum tile_pos tile) player objects state)
    (Bounce, _) ->
      Game lvls (MovingObject kind pos
        (vel_x, -2 * minObjSize) (accel_x, 0.0) animC animD) objects state
  where
    (MovingObject kind pos (vel_x, _) (accel_x, _) animC animD) = player
    levelNum = gameStateLvlNum state

-- -- | Apply gravity to the `MovingObject`.
-- applyGravity :: Float -> MovingObject -> MovingObject
-- applyGravity dt (MovingObject kind pos vel (accel_x, accel_y))
--   = MovingObject kind pos vel (accel_x, accel_y - g * dt)

-- | Apply gravity to the `MovingObject`.
applyGravityAsVel :: Float -> MovingObject -> MovingObject
applyGravityAsVel dt (MovingObject kind pos (vel_x, vel_y) accel animC animD)
  = MovingObject kind pos (vel_x, vel_y - g * dt) accel animC animD

-- | Apply friction to the `MovingObject`.
applyFriction :: Level -> MovingObject -> MovingObject
applyFriction lvl (MovingObject kind pos (vel_x, vel_y) accel animC animD)
  = MovingObject kind pos (vel_x * (1 - allFrictions), vel_y) accel animC animD
  where
    allFrictions = applyToParts (+) 0 takeFriction lvl (getSize kind) pos
    takeFriction level (tile_x, tile_y) =
      case takeElemFromMatrix level (mapPosToCoord (tile_x, tile_y - thresh)) of
        Nothing -> 0
        Just tile -> tileFrictionRate tile

-- | Check if the object can jump from this position.
canJump :: Level -> Position -> Bool
canJump lvl (pos_x, pos_y) =
  case (left_bot, right_bot) of
    (Just l_tile, Just r_tile) -> not (canPass l_tile && canPass r_tile)
    (Nothing, Nothing) -> False
    (Just l_tile, _) -> not $ canPass l_tile
    (_, Just r_tile) -> not $ canPass r_tile
  where
    left_bot = takeElemFromMatrix lvl (x, y)
    right_bot = takeElemFromMatrix lvl (x_r, y)
    (x, y) = mapPosToCoord (pos_x, pos_y - thresh)
    (x_r, _) = mapPosToCoord (pos_x + minObjSize, 0)

-- | Jump to the stars!
tryJump :: Level -> MovingObject -> Position -> MovingObject
tryJump lvl player@(MovingObject kind pos@(pos_x, pos_y) (vel_x, _) accel animC animD) (off_x, off_y)
  | checkForAnyPart canJump lvl (size_x, 1) pos && not inAir
    = MovingObject kind pos (vel_x + off_x, off_y) accel animC animD
  | otherwise = player
  where
    inAir = canMove lvl (pos_x + thresh, pos_y - thresh)
    (size_x, _) = (getSize kind)

-- | Updating the speed of Object due to user input.
changeSpeed :: MovingObject -> Vector2 -> MovingObject
changeSpeed (MovingObject kind pos (vel_x, vel_y) accel animC animD) (off_x, off_y)
  = MovingObject kind pos (vel_x + off_x, vel_y + off_y) accel animC animD

-- | Move Object due to it's velocity and acceleration.
move :: Float -> MovingObject -> MovingObject
move dt (MovingObject kind (pos_x, pos_y) (vel_x, vel_y) accel@(accel_x, accel_y) animC animD) =
  MovingObject kind (new_x, new_y) (vel_x + accel_x, vel_y + accel_y) accel animC animD
  where
    new_x = pos_x + vel_x * dt + accel_x * dt ** 2 / 2
    new_y = pos_y + vel_y * dt + accel_y * dt ** 2 / 2

-- | Apply all provided actions on the player.
performActions :: Level -> [Movement] -> MovingObject -> MovingObject
performActions lvl ms = foldr (.) id (map (performAction lvl) ms)

-- | Apply single action on the player.
performAction :: Level -> Movement -> MovingObject -> MovingObject
performAction lvl UP_BUTTON player = tryJump lvl player (0.0, snd step)
performAction _ DOWN_BUTTON player = player
performAction _ LEFT_BUTTON player = changeSpeed player (- fst step, 0.0)
performAction _ RIGHT_BUTTON player = changeSpeed player (fst step, 0.0)
performAction _ SPECIAL_BUTTON player = player

-- | Try to move the `MovingObject` by given offset.
tryMove :: Float -> Level -> MovingObject -> MovingObject
tryMove dt level object
  | canMoveAtThisLvl (new_x, new_y) = new_obj
  | canMoveAtThisLvl (old_x, new_y) && (isPlayer kind)
    = move dt (MovingObject kind old_pos (0.0, vel_y) (0.0, accel_y) updAnimC updAnimD)
  | canMoveAtThisLvl (old_x, new_y)
    = move dt (MovingObject kind old_pos (-vel_x, vel_y) (-accel_x, accel_y) updAnimC updAnimD)
  | canMoveAtThisLvl (new_x, old_y)
    = move dt (MovingObject kind old_pos (vel_x, 0.0) (accel_x, 0.0) updAnimC updAnimD)
  | isPlayer kind
    = MovingObject kind old_pos (0.0, 0.0) (0.0, 0.0) updAnimC updAnimD
  | otherwise
    = MovingObject kind old_pos (-vel_x, vel_y) (-accel_x, accel_y) updAnimC updAnimD
  where
    (MovingObject kind old_pos@(old_x, old_y)
      (vel_x, vel_y) (accel_x, accel_y) animC animD) = object
    canMoveAtThisLvl = checkforAllParts canMove level (getSize kind)
    new_obj@(MovingObject _ (new_x, new_y) _ _ _ _) = move dt object
    updAnimC = (mod' (animC + dt * animationScale) (getAnimDivisor kind))
    updAnimD
      | vel_y > 0 && vel_x > 0.6*tileSize = 7
      | vel_y > 0 && vel_x < -0.6*tileSize = 2
      | vel_y <= 0 && vel_x > 0.6*tileSize = 6
      | vel_y <= 0 && vel_x < -0.6*tileSize = 1
      | vel_y > 0 = if animD >= 5 then 7 else 2
      | otherwise = if animD >= 5 then 5 else 0

    isPlayer BigPlayer = True
    isPlayer SmallPlayer = True
    isPlayer _ = False

-- | Checks if the simple `MovingObject` can move at this position.
canMove :: Level -> Position -> Bool
canMove lvl pos@(pos_x, pos_y) =
  case (left_bot, left_top, right_bot, right_top) of
    (Just t1, Just t2, Just t3, Just t4) ->
      canPass t1 && canPass t2 && canPass t3 && canPass t4
    _ -> False
  where
    left_bot = takeElemFromMatrix lvl (x, y)
    left_top = takeElemFromMatrix lvl (x, y_r)
    right_bot = takeElemFromMatrix lvl (x_r, y)
    right_top = takeElemFromMatrix lvl (x_r, y_r)
    (x, y) = mapPosToCoord pos
    (x_r, y_r) = mapPosToCoord (pos_x + minObjSize, pos_y + minObjSize)

-- | Physics of the game.
updateGame :: (Int, Int) -> Float -> Game -> Game
updateGame res dt (Game lvls player objects state) =
  case gameStateNextLvlNum state of
    Nothing -> checkCollision (Game lvls upd_player upd_objects state)
    Just nextLevel -> Game lvls upd_player upd_objects
      (state {gameStateLvlNum = nextLevel, gameStateNextLvlNum = Nothing})
  where
    lvl = lvls !! gameStateLvlNum state -- TODO: make this is a safe way.
    (MovingObject _ plr_pos _ _ _ _) = player
    upd_player =
      ( tryMove dt lvl
      . applyFriction lvl
      . applyGravityAsVel dt
      . performActions lvl (S.toList (pressedKeys state))
      ) player
    upd_objects = updateObjects res dt lvl plr_pos objects

-- | Update objects due the current position of player.
-- If the object is far from the screen, doesn't update it.
updateObjects :: (Int, Int) -> Float -> Level -> Position -> [MovingObject] -> [MovingObject]
updateObjects _ _ _ _ [] = []
updateObjects res@(res_x, _) dt lvl plr_pos@(plr_pos_x, _) (obj:objs)
  | pos_x >= leftBoundary && pos_x <= rightBoundary
    = performAnimation dt ((tryMove dt lvl . applyGravityAsVel dt) obj)
      : updateObjects res dt lvl plr_pos objs
  | otherwise = obj : updateObjects res dt lvl plr_pos objs
  where
    (MovingObject _ (pos_x, _) _ _ _ _) = obj
    leftBoundary = plr_pos_x - (fromIntegral res_x)
    rightBoundary = plr_pos_x + (fromIntegral res_x)/2

animationScale::Float
animationScale = 6

getAnimDivisor:: Kind -> Float
getAnimDivisor kind = 1000

performAnimation :: Float -> MovingObject -> MovingObject
performAnimation dt obj@(MovingObject kind pos (velX, velY) acc animC animD) 
  = MovingObject kind pos (velX, velY) acc updAnimC animD
    where updAnimC = (mod' (animC + dt * animationScale) (getAnimDivisor kind))
          