{-# OPTIONS_GHC -Wall #-}

module Draw where

-- Start: Used in debug output
import Data.Fixed (mod')
import qualified Data.Set as S
-- End: Used in debug output
import Graphics.Gloss

import Lib

-- | Draw the game.
drawGame :: Assets -> ScreenSize -> Game -> Picture
drawGame assets res game =
  let
    curlvl = gameCurLevel game
    lvlMap = levelMap curlvl
    gameScale = getGameScale res lvlMap
    alivePlayers = getAlivePlayers (gamePlayers game)
    fres = (fromIntegral (fst res), fromIntegral (snd res))
    mapHeight = getMapHeight lvlMap
    textScale = textScaleFactor * gameScale
    charSize = 150 * textScale
    coordOffset = 2 * charSize
    floatOffset = 6 * charSize
    -- Debug output
    (MovingObject _ pos@(pos_x, pos_y) _ _ _ _) = playerObj (head (gamePlayers game))
    (сoord_x, coord_y) = mapPosToCoord pos
    (off_x, off_y) = (mod' pos_x tileSize, mod' pos_y tileSize)
    showScaledText str = scale textScale textScale (text str)
    showScaledObj n = (showScaledText . show) n
    showRow obj1 obj2 = showScaledObj obj1 <> translate 0 (-charSize) (showScaledObj obj2)
    debug = translate 0 (-gameScale * tileSize - charSize)
      ( showRow сoord_x coord_y
        <> translate coordOffset 0 (showRow pos_x pos_y)
        <> translate (coordOffset + floatOffset) 0 (showRow off_x off_y)
        <> translate 0  (-coordOffset) (pictures $ map (\(t, p) -> t p)
          (zip (map (\y -> translate 0.0 (-y * charSize)) [0..])
            (map showScaledObj (S.elems (pressedKeys game)))
          ))
      )
    -- End of Debug output.
    gameInfo =
      translate (- (fst fres) / 2 + charSize) 0 (
        showScaledText "lives:"
        <> pictures (map (\(p, pNum) ->
          translate 0 (-charSize * pNum) (showScaledObj (playerHp p))
          ) (zip (gamePlayers game) [1..])
      )) <> translate ((fst fres - charSize * 10) / 2) 0 (
        showScaledText (concat ["coins: ", show (gameCoins game)]
      ))
    composedRelative = scale gameScale gameScale $
      centerPictureY mapHeight 1
        ( translate (-(centerOfScreen res lvlMap alivePlayers)) 0 (
          drawLvl assets lvlMap
          <> pictures (map (drawObject assets) (levelObjs curlvl))
          <> pictures (map (drawObject assets . playerObj) alivePlayers)
          <> pictures (map (drawSprite assets) (levelSprites (gameCurLevel game)))
          )
        )
  in
    case gameNextLvlNum game of
      Nothing -> composedRelative
        <> translate (-(coordOffset + 2 * floatOffset) / 2) 0
          (centerPictureY mapHeight gameScale debug)
        <> translate 0 ((snd fres - coordOffset) / 2) gameInfo
      Just (-1) -> scale textScale textScale (
           ( translate (-charSize * 14) 0 (text "Choose the amount of players"))
          <> translate 0 (- charSize * 4) ((text . show) (length (gamePlayers game)))
        )
      Just _ -> scale textScale textScale (
          translate (-charSize * 4) 0 (text "You win!")
        )

-- | Draw the level.
drawLvl :: Assets -> LevelMap -> Picture
drawLvl _ [] = blank
drawLvl assets lvlMap
  = pictures $ map (\(t, p) -> t p)
  (zip (map (\y -> translate 0 (y * tileSize)) [0..])
    (map (drawLine assets) lvlMap))

-- | Draw the line of the level.
drawLine :: Assets -> [Tile] -> Picture
drawLine _ [] = blank
drawLine assets tiles
  = pictures $ map (\(t, p) -> t p)
  (zip (map (\y -> translate (y * tileSize) 0) [0..])
    (map (drawTile assets) (tiles)))

-- | Draw object.
drawObject :: Assets -> MovingObject -> Picture
drawObject assets (MovingObject kind (pos_x, pos_y) _ _ animC animD)
  = translate
    (pos_x + (size_x - tileSize) / 2)
    (pos_y + (size_y - tileSize) / 2)
    (drawKind assets kind animC animD)
  where
    (size_x, size_y) = getSize kind

-- | Draw object.
drawSprite :: Assets -> Sprite -> Picture
drawSprite assets (Sprite _ (pos_x, pos_y) animC _ _)
  = translate
    (pos_x + (size_x - tileSize) / 2)
    (pos_y + (size_y - tileSize) / 2)
    (drawCoinAnim (animSprites assets) animC)
  where
    (size_x, size_y) = (tileSize, tileSize * 4)

-- | Draw one tile.
drawTile :: Assets -> Tile -> Picture
drawTile assets TopBrick = getAssetFromList (envSprites assets) 0
drawTile assets MiddleBrick = getAssetFromList (envSprites assets) 10
drawTile assets BrickCoinBlock = getAssetFromList (envSprites assets) 0
drawTile assets BrickStarBlock = getAssetFromList (envSprites assets) 0
drawTile assets Ground = getAssetFromList (envSprites assets) 1
drawTile assets BonusBlockCoin = getAssetFromList (envSprites assets) 2
drawTile assets BonusBlockPowerUp = getAssetFromList (envSprites assets) 2
drawTile assets BonusBlockEmpty = getAssetFromList (envSprites assets) 3
drawTile assets PipeGreenTopLeft = getAssetFromList (envSprites assets) 4
drawTile assets PipeGreenTopRight = getAssetFromList (envSprites assets) 5
drawTile assets PipeGreenLeft = getAssetFromList (envSprites assets) 6
drawTile assets PipeGreenRight = getAssetFromList (envSprites assets) 7
drawTile assets RomboBlock = getAssetFromList (envSprites assets) 8
drawTile assets Coin = getAssetFromList (envSprites assets) 9
drawTile _ HiddenBlockLivesUp = blank
drawTile _ Empty = blank

-- | Draw the object kind.
drawKind :: Assets -> Kind -> Float -> Int -> Picture
drawKind assets (SmallPlayer 0) animC animD = drawPlayer (marioSprites assets) animC animD
drawKind assets (BigPlayer 0) animC animD   = drawPlayer (marioSpritesB assets) animC animD
drawKind assets (SmallPlayer 1) animC animD = drawPlayer (luigiSprites assets) animC animD
drawKind assets (BigPlayer 1) animC animD   = drawPlayer (luigiSpritesB assets) animC animD
drawKind assets (SmallPlayer 2) animC animD = drawPlayer (francescoSprites assets) animC animD
drawKind assets (BigPlayer 2) animC animD   = drawPlayer (francescoSpritesB assets) animC animD
drawKind _      (SmallPlayer _) animC animD = drawPlayer [] animC animD
drawKind _      (BigPlayer _) animC animD   = drawPlayer [] animC animD
drawKind assets Gumba animC _ = getAssetFromList (enemySprites assets) (0 + ((round animC) `mod` 2))
drawKind assets Turtle animC _ = getAssetFromList (enemySprites assets) (2 + ((round animC) `mod` 2))
drawKind assets Mushroom _ _ = getAssetFromList (enemySprites assets) 4
drawKind assets Star _ _ = getAssetFromList (enemySprites assets) 5
drawKind assets Shell _ _ = getAssetFromList (enemySprites assets) 6
drawKind assets HpMushroom _ _ = getAssetFromList (enemySprites assets) 7
drawKind assets Flagpole _ _ = getAssetFromList (enemySprites assets) 8

-- | Draw the player animation.
drawPlayer :: [Picture] -> Float -> Int -> Picture
drawPlayer mSprites animC 1 = getAssetFromList mSprites (2 + ((round animC) `mod` 4))
drawPlayer mSprites _ 0 = getAssetFromList mSprites 6
drawPlayer mSprites _ 5 = getAssetFromList mSprites 13
drawPlayer mSprites animC 6 = getAssetFromList mSprites (9 + ((round animC) `mod` 4))
drawPlayer mSprites _ 2 = getAssetFromList mSprites 0
drawPlayer mSprites _ 7 = getAssetFromList mSprites 7
drawPlayer mSprites _ 3 = getAssetFromList mSprites 1
drawPlayer mSprites _ 8 = getAssetFromList mSprites 8
drawPlayer mSprites _ _ = getAssetFromList mSprites 0

-- | Draw the animation of coin. TODO: Generalize.
drawCoinAnim :: [Picture] -> Float -> Picture
drawCoinAnim mSprites animC = getAssetFromList mSprites (0 + ((round animC) `mod` 14))

-- | Safely get the asset from the provided list.
-- If the asset is not found, return a placaholder picture.
getAssetFromList :: [Picture] -> Integer -> Picture
getAssetFromList assets num =
  case takeElemFromList assets num of
    Just p -> p
    Nothing -> scale tileSize tileSize (color black (rectangleSolid 1 1))

-- | Given picture height center it
centerPictureY :: Float -> Float -> Picture -> Picture
centerPictureY height gameScale pic = translate 0 (gameScale * (-height + tileSize) / 2) pic
