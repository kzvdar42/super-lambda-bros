{-# OPTIONS_GHC -Wall #-}

module App where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)

import Draw
import Handle
import Lib
import Update

-- | Read game maps from all specified source files.
readMaps :: (Char -> Tile) -> [String] -> IO [Level]
readMaps parse filenames = sequence $ map (readMap parse) filenames

-- | Read game map from the specified source file.
readMap :: (Char -> Tile) -> String -> IO Level
readMap parse filename = do
    content <- (readFile filename)
    let lvlmap = reverseList [[parse sym | sym <- l] | l <- lines content]
    let lvlObjs = getObjects (reverseList (lines content))
    return Level { levelMap = lvlmap, levelObjs = lvlObjs, levelInitPoint = (2, 2), levelSprites = []}
  where
    reverseList xs = foldl (\x y -> y:x) [] xs

getObjects :: [[Char]] -> [MovingObject] -- TODO: Look at me, this code is probably bad
getObjects inputMap = parseMaybe linesWithInds
  where
    linesWithInds :: [(Maybe Kind, Integer, Integer)]
    linesWithInds =
      concat ((zipWith
        (\l pos_y ->
          zipWith (\x pos_x -> (getObject x, pos_x, pos_y)) l [0..])) inputMap [0..])

    parseMaybe :: [(Maybe Kind, Integer, Integer)] -> [MovingObject]
    parseMaybe [] = []
    parseMaybe ((o, pos_x, pos_y):xs) = case o of
      Nothing -> parseMaybe xs
      Just k ->
        MovingObject k
          (fromIntegral pos_x * tileSize, fromIntegral pos_y * tileSize)
          (getInitSpeed k) (0, 0) 0 0 : parseMaybe xs

    getObject :: Char -> Maybe Kind
    getObject ch
      | ch == 'g' = Just Gumba
      | ch == 't' = Just Turtle
      | ch == 'F' = Just Flagpole
      | otherwise = Nothing

-- | Convert character from source file to the corresponding tile.
getTile :: Char -> Tile
getTile ch
  | ch == ' ' = Empty
  | ch == '1' = Ground
  | ch == '2' = Brick
  | ch == 'b' = BrickCoinBlock
  | ch == 'S' = BrickStarBlock
  | ch == '3' = BonusBlockCoin
  | ch == '4' = BonusBlockEmpty
  | ch == '5' = PipeGreenTopLeft
  | ch == '6' = PipeGreenTopRight
  | ch == '7' = PipeGreenLeft
  | ch == '8' = PipeGreenRight
  | ch == '9' = RomboBlock
  | ch == 'C' = Coin
  | ch == 'P' = BonusBlockPowerUp
  | ch == 'h' = HiddenBlockLivesUp
  | otherwise = Empty

-- | Program entry point.
run :: IO ()
run = do
  screenResolution <- getScreenSize
  print $ "Screen Resolution: " ++ (show screenResolution)
  spritesMario <- sequence $ map loadBMP (map (\x -> "assets/mario/mario_" ++ x ++ ".bmp") (map show [1..14 :: Integer]))
  spritesLuigi <- sequence $ map loadBMP (map (\x -> "assets/mario/luigi_" ++ x ++ ".bmp") (map show [1..14 :: Integer]))
  spritesFrancesco <- sequence $ map loadBMP (map (\x -> "assets/mario/francesco_" ++ x ++ ".bmp") (map show [1..14 :: Integer]))
  spritesMarioB <- sequence $ map loadBMP (map (\x -> "assets/mario/big_mario_" ++ x ++ ".bmp") (map show [1..14 :: Integer]))
  spritesLuigiB <- sequence $ map loadBMP (map (\x -> "assets/mario/big_luigi_" ++ x ++ ".bmp") (map show [1..14 :: Integer]))
  spritesFrancescoB <- sequence $ map loadBMP (map (\x -> "assets/mario/big_francesco_" ++ x ++ ".bmp") (map show [1..14 :: Integer]))

  spritesEnv <- sequence $ map loadBMP (map (\x -> "assets/environment/tile_" ++ x ++ ".bmp") (map show [1..9 :: Integer]))
  spritesEnemy <- sequence $ map loadBMP (map (\x -> "assets/enemies/enemy_" ++ x ++ ".bmp") (map show [1..9 :: Integer]))
  spritesAnim <- sequence $ map loadBMP (map (\x -> "assets/sprites/sprite_" ++ x ++ ".bmp") (map show [1..7 :: Integer]))

  maps <- (readMaps getTile ["assets/maps/map_1.txt"])

  let assets = Assets spritesMario spritesLuigi spritesFrancesco spritesMarioB spritesLuigiB spritesFrancescoB spritesEnv spritesEnemy spritesAnim
  play FullScreen (makeColorI 92 148 252 255) 60 (initGame maps) (drawGame assets screenResolution) handleGame (updateGame screenResolution)
