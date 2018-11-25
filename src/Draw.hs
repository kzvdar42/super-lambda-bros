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
drawGame assets res (Game _ curlvl player state) =
  let
    gameScale = getGameScale res lvlMap
    textScale = textScaleFactor * gameScale
    lvlMap = levelMap curlvl
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
      drawLvl assets lvlMap
      <> pictures (map (drawObject assets) (levelObjs curlvl))
      <> drawObject assets player)
    info = showScaledText (gameStateCoins state)
    fres = (fromIntegral (fst res), fromIntegral (snd res))
    mapHeight = getMapHeight lvlMap
    composedRelative = alignWorldToX ((*) gameScale $ fst pos)
      (getScreenOffset fres lvlMap gameScale) $ centerPictureY mapHeight gameScale composed
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

-- | Draw object.
drawObject :: Assets -> MovingObject -> Picture
drawObject assets (MovingObject kind (pos_x, pos_y) _ _ animC animD)
  = translate
    (pos_x + (size_x - minObjSize) / 2)
    (pos_y + (size_y - minObjSize) / 2)
    (drawKind assets kind animC animD)
  where
    (size_x, size_y) = getSize kind

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
drawTile _ Empty = blank

-- | Draw the object kind.
drawKind :: Assets -> Kind -> Float -> Int -> Picture
drawKind assets BigPlayer _ _ = getAssetFromList (marioSprites assets) 0
drawKind assets SmallPlayer animC animD = drawSmallPlayer (marioSprites assets) animC animD
drawKind assets Gumba animC _ = getAssetFromList (enemySprites assets) (0 + ((round animC) `mod` 2))
drawKind assets Turtle animC _ = getAssetFromList (enemySprites assets) (2 + ((round animC) `mod` 2))
drawKind assets Mushroom _ _ = getAssetFromList (enemySprites assets) 4
drawKind assets Star _ _ = getAssetFromList (enemySprites assets) 5
drawKind assets Shell _ _ = getAssetFromList (enemySprites assets) 6
drawKind assets HpMushroom _ _ = getAssetFromList (enemySprites assets) 7
drawKind assets Flagpole _ _ = getAssetFromList (enemySprites assets) 8


drawSmallPlayer :: [Picture] -> Float -> Int -> Picture
drawSmallPlayer mSprites animC 1 = getAssetFromList mSprites (2 + ((round animC) `mod` 4))
drawSmallPlayer mSprites _ 0 = getAssetFromList mSprites 6
drawSmallPlayer mSprites _ 5 = getAssetFromList mSprites 13
drawSmallPlayer mSprites animC 6 = getAssetFromList mSprites (9 + ((round animC) `mod` 4))
drawSmallPlayer mSprites _ 2 = getAssetFromList mSprites 0
drawSmallPlayer mSprites _ 7 = getAssetFromList mSprites 7
drawSmallPlayer mSprites _ _ = getAssetFromList mSprites 0

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

-- | Move the world accoring to player movement
alignWorldToX :: Float -> (Float, Float) -> Picture -> Picture
alignWorldToX x (offsetL, offsetR)
  | x < offsetL = translate (-offsetL) 0
  | x > offsetR = translate (-offsetR) 0
  | otherwise = translate (-x) 0

-- | Calculate offsets to limit map drawing on sides
getScreenOffset :: (Float, Float) -> LevelMap -> Float -> Position
getScreenOffset (width, _) lvlMap gameScale
  = (offset, size - offset - tileSize * gameScale)
  where
    offset = (width - tileSize * gameScale) / 2
    size = gameScale * tileSize * (fromIntegral $ length (lvlMap !! 0))
