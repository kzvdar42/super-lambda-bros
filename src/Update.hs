{-# OPTIONS_GHC -Wall #-}

module Update where

import qualified Data.Set as S
import Data.Fixed (mod')

import Lib

-- | Check if the player's head collides with some block.
-- And if is, run the `performCollisions`.
checkCollision :: Int -> Game -> Game
checkCollision playerNum game =
  collideAllMovingObjects $ case takeElemFromMatrix (levelMap curlvl) (x_close, y) of
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
        updlvl = (gameCurLevel game)
          { levelObjs = ((MovingObject objKind
              (mapCoordToPos (tile_x + off_x, tile_y + off_y))
              (1 * tileSize, 0) (0, 0) 0 0) : objects)
          }
      in
      game {gameCurLevel = updlvl}
    (Change tile, tile_pos) ->
      game {gameCurLevel = updtile tile_pos tile}
    (Bounce, _) ->
      let
        upd_player = curPlayer {playerObj =
          (MovingObject kind pos (vel_x, -2 * minObjSize) (accel_x, 0.0) animC animD)}
      in
      game {gamePlayers = (updateElemInList (gamePlayers game) upd_player (fromIntegral playerNum))}
    (CollectCoin, tile_pos) -> (addCoinToLevel tile_pos . incrementCoins) game
    (Die, _) -> game
      { gamePlayers =
        updateElemInList
        (gamePlayers game)
        (createPlayer (levelInitPoint initlvl) ((playerHp curPlayer) - 1) True)
        (fromIntegral playerNum)
      }
  where
    initlvl = (gameLevels game) !! (gameLvlNum game)
    curPlayer = (gamePlayers game) !! playerNum
    curlvl = gameCurLevel game
    objects = levelObjs curlvl
    (MovingObject kind pos (vel_x, _) (accel_x, _) animC animD) =
      playerObj ((gamePlayers game) !! playerNum)
    updtile t_pos t =
      curlvl {levelMap = updateElemInMatrix (levelMap curlvl) t_pos t}

-- | Reset the current level.
resetLevel :: Game -> Game
resetLevel game = game
  { gameCurLevel = initlvl
  , gamePlayers = map
      (\p -> createPlayer (levelInitPoint initlvl) (playerHp p) (playerHp p <= 0))
      (gamePlayers game)
  }
  where
    initlvl = (gameLevels game) !! (gameLvlNum game)


addCoinToLevel :: Coord -> Game -> Game
addCoinToLevel (coord_x, coord_y) game = game {gameCurLevel = updLvl}
      where
        pos = mapCoordToPos (coord_x, coord_y + 1)
        coinSprite = Sprite CoinSprite pos 0 6 [SelfDestroy]
        updLvlSprites = coinSprite : levelSprites currLvl
        currLvl = gameCurLevel game
        updLvl = currLvl {levelSprites = updLvlSprites}

-- | Increments the number of coins.
incrementCoins :: Game -> Game
incrementCoins game
  | coins < 99 = game { gameCoins = coins + 1 }
  | otherwise  = game { gamePlayers = upd_players
                      , gameCoins = coins - 99 }
  where
    coins = gameCoins game
    upd_players = map (\p -> p {playerHp = playerHp p + 1}) (gamePlayers game)

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
performActions :: Int -> LevelMap -> [Movement] -> MovingObject -> MovingObject
performActions playerNum lvl ms = foldr (.) id (map (performAction playerNum lvl) ms)

-- | Apply single action on the player.
performAction :: Int -> LevelMap -> Movement -> MovingObject -> MovingObject
performAction 0 lvl P1_U_BUTTON player = tryJump lvl player (0.0, snd step)
performAction 0 _   P1_L_BUTTON player = changeSpeed player (- fst step, 0.0)
performAction 0 _   P1_D_BUTTON player = player
performAction 0 _   P1_R_BUTTON player = changeSpeed player (fst step, 0.0)
performAction 1 lvl P2_U_BUTTON player = tryJump lvl player (0.0, snd step)
performAction 1 _   P2_L_BUTTON player = changeSpeed player (- fst step, 0.0)
performAction 1 _   P2_D_BUTTON player = player
performAction 1 _   P2_R_BUTTON player = changeSpeed player (fst step, 0.0)
performAction 2 lvl P3_U_BUTTON player = tryJump lvl player (0.0, snd step)
performAction 2 _   P3_L_BUTTON player = changeSpeed player (- fst step, 0.0)
performAction 2 _   P3_D_BUTTON player = player
performAction 2 _   P3_R_BUTTON player = changeSpeed player (fst step, 0.0)
performAction _ _ _ player = player

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
updateAnimation dt lvlMap (MovingObject kind pos vel@(vel_x, _) accel@(_, _) animC animD)
  | isPlayer kind =
    MovingObject kind pos vel accel (mod' (animC + dt * animationScale) (getAnimDivisor kind)) updAnimD
  | otherwise =
    MovingObject kind pos vel accel (mod' (animC + dt * 2) (getAnimDivisor kind)) updAnimD
  where
    canJ = canObjJump lvlMap (getSize kind) pos
    updAnimD
      | not canJ && vel_x > 0.6 * tileSize = 7
      | not canJ && vel_x < -0.6 * tileSize = 2
      | not canJ = if animD >= 5 then 7 else 2
      -- | accel_x == 0 = 8
      -- | accel_x > 0 && vel_x < 0 = 3
      | vel_x > 0.6 * tileSize = 6
      | vel_x < -0.6 * tileSize = 1
      | otherwise = if animD >= 5 then 5 else 0

-- | Physics of the game.
updateGame :: ScreenSize -> Float -> Game -> Game
updateGame res dt game =
  case gameNextLvlNum game of
    Nothing ->
      if length alivePlayers == 0
        then resetLevel game
        else foldr (.) id (map checkCollision (take (length players) [0..])) upd_game
    Just nextLevel -> game -- TODO: move to next level.
  where
    curlvl = gameCurLevel game
    lvlMap = levelMap curlvl
    updlvl = curlvl { levelObjs = upd_objects, levelSprites = upd_sprites }
    upd_game = game { gameCurLevel = updlvl, gamePlayers = upd_players }
    players = gamePlayers game
    alivePlayers = filter (\p -> not (playerIsDead p)) players
    screen_pos = centerOfScreen alivePlayers
    upd_players = map
      (\(p, p_num) ->
        if (playerIsDead p)
          then p
          else updatePlayer res screen_pos dt lvlMap (S.toList (pressedKeys game)) p_num p
      )
      (zip players [0..])
    upd_objects = updateObjects res dt lvlMap screen_pos (levelObjs curlvl)
    upd_sprites = updateSprites dt (levelSprites curlvl)


tryMoveInScreen :: Float -> ScreenSize -> LevelMap -> Float -> MovingObject -> MovingObject
tryMoveInScreen dt res@(res_x, _) lvlMap centerOfScreenPos object
  | new_x < centerOfScreenPos - boundary || new_x > centerOfScreenPos + boundary =
    tryMove dt lvlMap (MovingObject k pos (0, vel_y) (0, accel_y) anC anD)
  | otherwise = upd_object
  where
    (MovingObject k pos (_, vel_y) (_, accel_y) anC anD) = object
    upd_object@(MovingObject _ (new_x, _) _ _ _ _) = tryMove dt lvlMap object
    boundary = fromIntegral res_x / (2 * gameScale) - tileSize/2
    gameScale = getGameScale res lvlMap

-- | Update player state.
updatePlayer :: ScreenSize -> Float -> Float -> LevelMap -> [Movement] -> Int -> Player -> Player
updatePlayer res centerOfScreenPos dt lvlMap movements playerNum player = player
  { playerObj =
    ( updateAnimation dt lvlMap
    . tryMoveInScreen dt res lvlMap centerOfScreenPos
    . applyFriction lvlMap
    . applyGravityAsVel dt
    . performActions playerNum lvlMap movements
    ) (playerObj player)
  }

-- | Update objects due the current position of player.
-- If the object is falled down, delete it.
-- If the object is far from the screen, doesn't update it.
updateObjects :: ScreenSize -> Float -> LevelMap -> Float -> [MovingObject] -> [MovingObject]
updateObjects _ _ _ _ [] = []
updateObjects res@(res_x, _) dt lvlMap centerOfScreenPos (obj:objs)
  | pos_y < - snd (getSize kind) = updateObjects res dt lvlMap centerOfScreenPos objs
  | pos_x >= leftBoundary && pos_x <= rightBoundary
    = ((updateAnimation dt lvlMap . tryMove dt lvlMap . applyGravityAsVel dt) obj)
      : updateObjects res dt lvlMap centerOfScreenPos objs
  | otherwise = obj : updateObjects res dt lvlMap centerOfScreenPos objs
  where
    (MovingObject kind (pos_x, pos_y) _ _ _ _) = obj
    leftBoundary = centerOfScreenPos - (fromIntegral res_x) / (2 / 3 * gameScale)
    rightBoundary = centerOfScreenPos + (fromIntegral res_x) / gameScale
    gameScale = getGameScale res lvlMap

updateSprites :: Float -> [Sprite] -> [Sprite]
updateSprites _ [] = []
updateSprites dt (s:sp) = if ttl < counter then others else updSprite : others
  where
    (Sprite typ pos counter ttl act) = s
    updSprite = (Sprite typ pos (counter + dt * animationScale * 2.5) ttl act)
    others = (updateSprites dt sp)

collideAllMovingObjects :: Game -> Game
collideAllMovingObjects game =
  game { gamePlayers = updatedPlayers, gameCurLevel = updatedLevel }
  where
    curLvl = gameCurLevel game
    updatedPlayers = zipWith (\o n -> o { playerObj = n }) (gamePlayers game) updatedPlayerObjs
    updatedLevel = curLvl { levelObjs = updatedLevelObjs }
    enemies = levelObjs curLvl
    playerObjs = map playerObj (gamePlayers game)
    (updatedPlayerObjs, updatedLevelObjs) = collidePlayersAndEnemies playerObjs enemies

collidePlayersAndEnemies
  :: [MovingObject] -- ^ List of players
  -> [MovingObject] -- ^ List of enemies
  -> ([MovingObject], [MovingObject])
collidePlayersAndEnemies players enemies = (updatedPlayers, updatedEnemies)
  where
    (updatedPlayers, halfUpdatedEnemies) = collideEachWithClosest players enemies
    (updatedEnemies, _) = collideEachWithClosest halfUpdatedEnemies halfUpdatedEnemies

collideEachWithClosest
  :: [MovingObject] -- ^ List of objects to perform collision of
  -> [MovingObject] -- ^ List of objects to perform collision with closest of them
  -> ([MovingObject], [MovingObject])
collideEachWithClosest []        []     = ([], [])
collideEachWithClosest toCollide []     = (toCollide, [])
collideEachWithClosest []        others = ([], others)
collideEachWithClosest toCollide others =
  ( firstCollided : nextToCollide
  , secondCollided : nextOthers
  )
  where
    (nextToCollide, nextOthers) = collideEachWithClosest othersToCollide shorterOthers
    (collideIt : othersToCollide) = toCollide
    (firstCollided, secondCollided) = performCollision collideIt (others !! closestIndex)
    closestIndex = findClosestIndex collideIt others
    shorterOthers = removeByIndex closestIndex others

    findClosestIndex :: MovingObject -> [MovingObject] -> Int
    findClosestIndex _ [] = error "Could not find index in an empty array"
    findClosestIndex obj (h:t) = findBestElIndex (distance obj) 1 0 (distance obj h) t
      where
        distance mo1 mo2 = sqrt ((x1 - x2) ^^ 2 + (y1 - y2) ^^ 2)
          where
            (MovingObject _ (x1, y1) _ _ _ _) = mo1
            (MovingObject _ (x2, y2) _ _ _ _) = mo2

        findBestElIndex _ _ bestInd _ [] = bestInd
        findBestElIndex f curInd bestInd bestEl (h':t') =
          findBestElIndex f (curInd + 1) betterInd betterEl t'
          where
            curEl = f h'
            (betterInd, betterEl) =
              if bestEl < curEl
                then (curInd, curEl)
                else (bestInd, bestEl)

    removeByIndex :: Int -> [a] -> [a]
    removeByIndex _ [] = []
    removeByIndex i (h:t)
      | i == 0 = t
      | otherwise = h : removeByIndex (i - 1) t

performCollision :: MovingObject -> MovingObject -> (MovingObject, MovingObject)
performCollision fstObj secObj = (fstObj, secObj) -- TODO
