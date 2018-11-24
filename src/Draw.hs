{-# OPTIONS_GHC -Wall #-}

module Draw where

import Data.Fixed (mod') -- TODO remove
import qualified Data.Set as S -- TODO remove
import Graphics.Gloss

import Lib

-- | Convert a pair of integers into a pair of floats.
getFloating :: (Int, Int) -> (Float, Float)
getFloating (a, b) = (fromIntegral a, fromIntegral b)

-- | Draw the game.
drawGame :: Assets -> (Int, Int) -> Game -> Picture
drawGame assets res game@(Game levels player state) =
  let
    gameScale = gameScaleFactor * (snd fres) / mapHeight
    textScale = textScaleFactor * gameScale
    lvl = levels !! (gameStateLvlNum state) -- TODO: do this in a safe way
    -- Debug output
    (MovingObject _ pos@(pos_x, pos_y) _ _ _ _) = player
    (сoord_x, coord_y) = mapPosToCoord pos
    (off_x, off_y) = (mod' pos_x tileSize, mod' pos_y tileSize)
    charSize = 150 * textScale
    coordOffset = 2 * charSize
    floatOffset = 6 * charSize
    showScaledText str = scale textScale textScale (text (show str))
    inputEvents = pictures $ map (\(t, p) -> t p)
      (zip (map (\y -> translate 0.0 (-y * charSize)) [0..])
        (map (showScaledText) (S.elems (pressedKeys state)))
      )
    debug = translate 0 (-gameScale * tileSize - charSize)
      (
        showScaledText сoord_x <> translate 0 (-charSize) (showScaledText coord_y)
        <> translate coordOffset 0
          (showScaledText pos_x <> translate 0 (-charSize) (showScaledText pos_y))
        <> translate (coordOffset + floatOffset) 0
          (showScaledText off_x <> translate 0 (-charSize) (showScaledText off_y))
        <> translate 0  (-coordOffset) inputEvents
      )
    composed = scale gameScale gameScale (
      drawLvl assets (levelMap lvl)
      <> pictures (map (drawObject assets) (levelObjs lvl))
      <> drawObject assets player)
    info = showScaledText (gameStateCoins state)
    fres = getFloating res
    mapHeight = getMapHeight game
    composedRelative = alignWorldToX ((*) gameScale $ fst pos)
      (getScreenOffset fres game gameScale) $ centerPictureY mapHeight gameScale composed
  in
    composedRelative
    <> translate (-(coordOffset + 2 * floatOffset) / 2) 0
      (centerPictureY mapHeight gameScale debug)
    <> translate ((fst fres - coordOffset) / 2) 
      ((snd fres - coordOffset) / 2) info

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

-- | Draw one tile.
drawTile :: Assets -> Tile -> Picture
drawTile assets Brick = getAssetFromList (envSprites assets) 0
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
drawTile _ Empty = blank--color (makeColorI 92 148 252 255) (rectangleSolid tileSize tileSize)

-- | Draw the object kind.
drawKind :: Assets -> Kind -> Float -> Int -> Picture
drawKind assets BigPlayer animC animD = getAssetFromList (marioSprites assets) 0
drawKind assets SmallPlayer animC animD = drawSmallPlayer assets animC animD--text (show animD)--getAssetFromList (marioSprites assets) 0 --drawSmallPlayer assets animC animD
drawKind assets Gumba animC animD = getAssetFromList (enemySprites assets) (0 + ((round animC) `mod` 2))
drawKind assets Turtle animC animD = getAssetFromList (enemySprites assets) (2 + ((round animC) `mod` 2))
drawKind assets Mushroom animC animD = getAssetFromList (enemySprites assets) 4
drawKind assets Shell animC animD = getAssetFromList (enemySprites assets) 6
drawKind assets Star animC animD = getAssetFromList (enemySprites assets) 5
drawKind assets HpMushroom animC animD = getAssetFromList (enemySprites assets) 5

-- drawKind assets Star = getAssetFromList (enemySprites assets) 3
-- drawKind assets Shell = getAssetFromList (enemySprites assets) 4
-- drawKind assets HpMushroom = getAssetFromList (enemySprites assets) 5

drawSmallPlayer::Assets->Float->Int->Picture
drawSmallPlayer assets animC 1 = getAssetFromList (marioSprites assets) (2 + ((round animC) `mod` 4))
drawSmallPlayer assets animC 0 = getAssetFromList (marioSprites assets) 6
drawSmallPlayer assets animC 5 = getAssetFromList (marioSprites assets) 13
drawSmallPlayer assets animC 6 = getAssetFromList (marioSprites assets) (9 + ((round animC) `mod` 4))
drawSmallPlayer assets animC 2 = getAssetFromList (marioSprites assets) 0
drawSmallPlayer assets animC 7 = getAssetFromList (marioSprites assets) 7
drawSmallPlayer assets _ _ = getAssetFromList (marioSprites assets) 0


-- | Safely get the asset from the provided list.
-- If the asset is not found, return a placaholder picture.
getAssetFromList :: [Picture] -> Integer -> Picture
getAssetFromList assets num =
  case takeElemFromList assets num of
    Just p -> p
    Nothing -> scale tileSize tileSize (color black (rectangleSolid 1 1))

-- | Draw object.
drawObject :: Assets -> MovingObject -> Picture
drawObject assets (MovingObject kind (pos_x, pos_y) _ _ animC animD)
  = translate
    (pos_x + (size_x - minObjSize) / 2)
    (pos_y + (size_y - minObjSize) / 2)
    (drawKind assets kind animC animD)
  where
    (size_x, size_y) = getSize kind

-- | Based on tile count of stored map calculate map size
getMapHeight :: Game -> Float
getMapHeight (Game lvls _ state) = len * tileSize
  where
    len = fromIntegral (length (levelMap (lvls !! gameStateLvlNum state))) -- TODO: do this safe

-- | Given picture height center it
centerPictureY :: Float -> Float -> Picture -> Picture
centerPictureY height gameScale pic = translate 0 (gameScale * (-height + tileSize) / 2) pic

-- | Move the world accoring to player movement
alignWorldToX :: Float -> (Float, Float) -> Picture -> Picture
alignWorldToX x (offsetL, offsetR)
  | x < offsetL = translate (-offsetL) 0
  | x > offsetR = translate (-offsetR) 0
  | otherwise = translate (-x) 0

-- | Calculate offsets to limit map drawing on sides
getScreenOffset :: (Float, Float) -> Game -> Float -> Position
getScreenOffset (width, _) (Game levels _ state) gameScale
  = (offset, size - offset - tileSize * gameScale)
  where
    offset = (width - tileSize * gameScale) / 2
    size = gameScale * tileSize * (fromIntegral $ length (lvlMap !! 0))
    lvlMap = levelMap (levels !! gameStateLvlNum state)
