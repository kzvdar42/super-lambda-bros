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
readMap :: (Char -> Tile) -> String -> IO [[Tile]]
readMap parse filename = do
    content <- (readFile filename)
    let tilemap = reverseList [[parse sym | sym <- line] | line <- lines content]
    return tilemap
  where
    reverseList xs = foldl (\x y -> y:x) [] xs

-- | Convert character from source file to the corresponding tile.
getTile :: Char -> Tile
getTile num
  | num == ' ' = Empty
  | num == '1' = Ground
  | num == '2' = Brick
  | num == '3' = BonusBlockActive
  | num == '4' = BonusBlockEmpty
  | num == '5' = PipeGreenTopLeft
  | num == '6' = PipeGreenTopRight
  | num == '7' = PipeGreenLeft
  | num == '8' = PipeGreenRight
  | num == '9' = RomboBlock
  | otherwise = Empty

-- | Program entry point.
run :: IO ()
run = do
  screenResolution <- getScreenSize
  print $ "Screen Resolution: " ++ (show screenResolution)
  marioSprites <- sequence $ map loadBMP (map (\x -> "assets/mario/mario_" ++ x ++ ".bmp") (map show [1..2]))
  envSprites <- sequence $ map loadBMP (map (\x -> "assets/environment/tile_" ++ x ++ ".bmp") (map show [1..9]))
  enemySprites <- sequence $ map loadBMP (map (\x -> "assets/enemies/enemy_" ++ x ++ ".bmp") (map show [1..5]))

  maps <- (readMaps getTile ["assets/maps/map_1.txt"])

  let assets = Assets marioSprites envSprites enemySprites
  play FullScreen (makeColor (92/255) (148/255) (252/255) (255/255)) 60 (initGame maps) (drawGame assets screenResolution) handleGame (updateGame screenResolution)
