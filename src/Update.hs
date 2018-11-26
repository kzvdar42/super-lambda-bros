{-# OPTIONS_GHC -Wall #-}

module Update where

import qualified Data.Set as S
import Data.Fixed (mod')

import Lib

-- | Check if the player's head collides with some block.
-- And if is, run the `performCollisions`.
checkCollision :: Int -> Game -> Game
checkCollision playerNum game =
  case takeElemFromMatrix (levelMap curlvl) (x_close, y) of
    Nothing -> case takeElemFromMatrix (levelMap curlvl) (x_far, y) of
      Nothing -> if pos_y < 0 then performCollisions playerNum [(Die, (x, y))] game else game
      Just tile -> performCollisions playerNum (map (\c -> (c, (x_far, y))) (typeOfCollision tile)) game
    Just tile -> performCollisions playerNum (map (\c -> (c, (x_close, y))) (typeOfCollision tile)) game
  where
    curlvl = gameCurLevel game
    (MovingObject kind (pos_x, pos_y) _ _ _ _) = playerObj ((gamePlayers game) !! playerNum)
    (x, y) = mapPosToCoord (pos_x, pos_y + (snd (getSize kind)) + thresh)
    (x_r, _) = mapPosToCoord (pos_x + (fst (getSize kind)), pos_y)
    (x_close, x_far) =
      if pos_x - (fromIntegral x) * tileSize < (fromIntegral x_r) * tileSize - pos_x
        then (x, x_r)
        else (x_r, x)

-- | Perform the collisions.
-- Right now the implementation is fixed to the position of the player.
performCollisions :: Int -> [(CollisionType, Coord)] -> Game -> Game
performCollisions _ [] game = game
performCollisions playerNum (c:cs) game =
  performCollisions playerNum cs $ case c of
    (Delete, tile_pos) -> game {gameCurLevel = updtile tile_pos Empty}
    (Spawn objKind (off_x, off_y), (tile_x, tile_y)) ->
      let
        updlvl = Level
          { levelMap = levelMap curlvl
          , levelInitPoint = levelInitPoint curlvl
          , levelObjs = ((MovingObject objKind
              (mapCoordToPos (tile_x + off_x, tile_y + off_y))
              (1.0 * tileSize, 0.0) (0.0, 0.0) 0 0) : objects)
          }
      in
      game {gameCurLevel = updlvl}
    (Change tile, tile_pos) ->
      game {gameCurLevel = updtile tile_pos tile}
    (Bounce, _) ->
      let
        plr = (gamePlayers game) !! playerNum
        upd_player = plr {playerObj =
          (MovingObject kind pos (vel_x, -2 * minObjSize) (accel_x, 0.0) animC animD)}
      in
      game {gamePlayers = (updateElemInList (gamePlayers game) upd_player (fromIntegral playerNum))}
    (CollectCoin, _) -> incrementCoins game
    (Die, _) -> game -- TODO: Decrease the Hp.
      { gameCurLevel = initlvl
      , gamePlayers =
        [ (initPlayer (levelInitPoint initlvl) 3)
        , (initPlayer (levelInitPoint initlvl) 3)
        ]
        -- initPlayer (levelInitPoint initlvl) (playerHp (gamePlayer game) - 1)
      }
  where
    curlvl = gameCurLevel game
    objects = levelObjs curlvl
    initlvl = (gameLevels game) !! (gameLvlNum game)
    (MovingObject kind pos (vel_x, _) (accel_x, _) animC animD) =
      playerObj ((gamePlayers game) !! playerNum)
    updtile t_pos t =
      curlvl {levelMap = updateElemInMatrix (levelMap curlvl) t_pos t}

-- | Increments the number of coins.
incrementCoins :: Game -> Game
incrementCoins game
  | coins < 99 = game { gameCoins = coins + 1 }
  | otherwise   = game { gamePlayers = upd_players
                       , gameCoins = coins - 99 }
  where 
    coins = gameCoins game
    upd_players =
      [ ((gamePlayers game) !! 0) {playerHp = playerHp ((gamePlayers game) !! 0) + 1}
      , ((gamePlayers game) !! 1) {playerHp = playerHp ((gamePlayers game) !! 1) + 1}
      ]

-- | Apply gravity to the `MovingObject`.
applyGravityAsVel :: Float -> MovingObject -> MovingObject
applyGravityAsVel dt (MovingObject kind pos (vel_x, vel_y) accel animC animD)
  = MovingObject kind pos (vel_x, vel_y - g * dt) accel animC animD

-- | Apply friction to the `MovingObject`.
applyFriction :: LevelMap -> MovingObject -> MovingObject
applyFriction lvl (MovingObject kind pos (vel_x, vel_y) accel animC animD)
  = MovingObject kind pos (vel_x * (1 - allFrictions), vel_y) accel animC animD
  where
    allFrictions = applyToParts (+) 0 takeFriction lvl (getSize kind) pos
    takeFriction level (tile_x, tile_y) =
      case takeElemFromMatrix level (mapPosToCoord (tile_x, tile_y - thresh)) of
        Nothing -> tileFrictionRate Empty
        Just tile -> tileFrictionRate tile

-- | Check if the object can jump from this position.
canJump :: LevelMap -> Position -> Bool
canJump lvlMap (pos_x, pos_y) =
  case (left_bot, right_bot) of
    (Just l_tile, Just r_tile) -> not (canPass l_tile && canPass r_tile)
    (Just l_tile, _) -> not $ canPass l_tile
    (_, Just r_tile) -> not $ canPass r_tile
    (Nothing, Nothing) -> False
  where
    left_bot = takeElemFromMatrix lvlMap (x, y)
    right_bot = takeElemFromMatrix lvlMap (x_r, y)
    (x, y) = mapPosToCoord (pos_x + thresh, pos_y - thresh)
    (x_r, _) = mapPosToCoord (pos_x + minObjSize - thresh, 0)

-- | CanJump for MovingObjects.
canObjJump :: LevelMap -> Size -> Position -> Bool
canObjJump lvlMap (size_x, _) (pos_x, pos_y) =
  checkForAnyPart canJump lvlMap (size_x, minObjSize) (pos_x, pos_y)

-- | Jump to the stars!
tryJump :: LevelMap -> MovingObject -> Position -> MovingObject
tryJump lvlMap player@(MovingObject kind pos (vel_x, _) accel animC animD) (off_x, off_y)
  | canObjJump lvlMap (getSize kind) pos
    = MovingObject kind pos (vel_x + off_x, off_y) accel animC animD
  | otherwise = player
    

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
performActions :: LevelMap -> [Movement] -> MovingObject -> MovingObject
performActions lvl ms = foldr (.) id (map (performAction lvl) ms)

performActions2 :: LevelMap -> [Movement] -> MovingObject -> MovingObject
performActions2 lvl ms = foldr (.) id (map (performAction2 lvl) ms)

-- | Apply single action on the player.
performAction :: LevelMap -> Movement -> MovingObject -> MovingObject
performAction lvl UP_BUTTON player = tryJump lvl player (0.0, snd step)
performAction _ DOWN_BUTTON player = player
performAction _ LEFT_BUTTON player = changeSpeed player (- fst step, 0.0)
performAction _ RIGHT_BUTTON player = changeSpeed player (fst step, 0.0)
performAction _ SPECIAL_BUTTON player = player
performAction _ _ player = player

performAction2 :: LevelMap -> Movement -> MovingObject -> MovingObject
performAction2 lvl W_BUTTON player = tryJump lvl player (0.0, snd step)
performAction2 _ A_BUTTON player = changeSpeed player (- fst step, 0.0)
performAction2 _ D_BUTTON player = changeSpeed player (fst step, 0.0)
performAction2 _ _ player = player

-- | Try to move the `MovingObject` by given offset.
tryMove :: Float -> LevelMap -> MovingObject -> MovingObject
tryMove dt level object
  | canMoveAtThisLvl (new_x, new_y) = new_obj
  | canMoveAtThisLvl (old_x, new_y) && (isPlayer kind)
    = move dt (MovingObject kind old_pos (0.0, vel_y) (0.0, accel_y) animC animD)
  | canMoveAtThisLvl (old_x, new_y)
    = move dt (MovingObject kind old_pos (-vel_x, vel_y) (-accel_x, accel_y) animC animD)
  | canMoveAtThisLvl (new_x, old_y)
    = move dt (MovingObject kind old_pos (vel_x, 0.0) (accel_x, 0.0) animC animD)
  | isPlayer kind
    = MovingObject kind old_pos (0.0, 0.0) (0.0, 0.0) animC animD
  | otherwise
    = MovingObject kind old_pos (-vel_x, vel_y) (-accel_x, accel_y) animC animD
  where
    (MovingObject kind old_pos@(old_x, old_y)
      (vel_x, vel_y) (accel_x, accel_y) animC animD) = object
    canMoveAtThisLvl = checkforAllParts canMove level (getSize kind)
    new_obj@(MovingObject _ (new_x, new_y) _ _ _ _) = move dt object

-- | Checks if the simple `MovingObject` can move at this position.
canMove :: LevelMap -> Position -> Bool
canMove lvl (pos_x, pos_y) =
  foldr ((&&) . canM) True [left_bot, left_top, right_bot, right_top]
  where
    canM maybeT = case maybeT of
      Nothing -> True
      Just t -> canPass t
    left_bot = takeElemFromMatrix lvl (x, y)
    left_top = takeElemFromMatrix lvl (x, y_r)
    right_bot = takeElemFromMatrix lvl (x_r, y)
    right_top = takeElemFromMatrix lvl (x_r, y_r)
    (x, y) = mapPosToCoord (pos_x + thresh, pos_y)
    (x_r, y_r) = mapPosToCoord (pos_x + minObjSize - thresh, pos_y + minObjSize)

-- | Update the animation state of object.
updateAnimation :: Float -> LevelMap -> MovingObject -> MovingObject
updateAnimation dt lvlMap (MovingObject kind pos vel@(vel_x, _) accel@(_, accel_y) animC animD)
  | isPlayer kind =
    MovingObject kind pos vel accel (mod' (animC + dt * animationScale) (getAnimDivisor kind)) updAnimD
  | otherwise =
    MovingObject kind pos vel accel (mod' (animC + dt*2) (getAnimDivisor kind)) updAnimD
  where
    canJ = canObjJump lvlMap (getSize kind) pos
    updAnimD
      | not canJ && vel_x > 0.6 * tileSize = 7
      | not canJ && vel_x < -0.6 * tileSize = 2
      | accel_y <= 0 && vel_x > 0.6 * tileSize = 6
      | accel_y <= 0 && vel_x < -0.6 * tileSize = 1
      | not canJ = if animD >= 5 then 7 else 2
      | otherwise = if animD >= 5 then 5 else 0

-- | Physics of the game.
updateGame :: ScreenSize -> Float -> Game -> Game
updateGame res dt game =
  case gameNextLvlNum game of
    Nothing ->
      checkCollision 1 $ checkCollision 0 game { gameCurLevel = updlvl, gamePlayers = upd_players }
    Just nextLevel -> game -- TODO:  move to next level.
  where
    curlvl = gameCurLevel game
    lvlMap = levelMap curlvl
    updlvl = curlvl { levelObjs = upd_objects }
    (MovingObject _ plr_pos _ _ _ _) = playerObj (head players)
    players = gamePlayers game
    upd_players =
      [ (head players) {playerObj = ((updatePlayer dt lvlMap
      . performActions lvlMap (S.toList (pressedKeys game))
      ) (playerObj (head players)))}
      , (players !! 1) 
        {playerObj =
          (updatePlayer dt lvlMap
          . performActions2 lvlMap (S.toList (pressedKeys game))) (playerObj (players !! 1))
        }
      ]
    upd_objects = updateObjects res dt lvlMap plr_pos (levelObjs curlvl)

-- | Update player state.
updatePlayer :: Float -> LevelMap -> MovingObject -> MovingObject
updatePlayer dt lvlMap =
  updateAnimation dt lvlMap . tryMove dt lvlMap . applyFriction lvlMap . applyGravityAsVel dt

-- | Update objects due the current position of player.
-- If the object is far from the screen, doesn't update it.
updateObjects :: ScreenSize -> Float -> LevelMap -> Position -> [MovingObject] -> [MovingObject]
updateObjects _ _ _ _ [] = []
updateObjects res@(res_x, _) dt lvlMap plr_pos@(plr_pos_x, _) (obj:objs)
  | pos_x >= leftBoundary && pos_x <= rightBoundary
    = ((updateAnimation dt lvlMap . tryMove dt lvlMap . applyGravityAsVel dt) obj)
      : updateObjects res dt lvlMap plr_pos objs
  | otherwise = obj : updateObjects res dt lvlMap plr_pos objs
  where
    (MovingObject _ (pos_x, _) _ _ _ _) = obj
    leftBoundary = plr_pos_x - (fromIntegral res_x) / (2 / 3 * gameScale)
    rightBoundary = plr_pos_x + (fromIntegral res_x) / gameScale
    gameScale = getGameScale res lvlMap
