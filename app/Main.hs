
-- | Visibility on the 2D plane.
--   Uses an instance of Warnocks algorithm.
--   TODO: animate the line segments, make them spin and move around so we can see
--         that it's a dynamic visiblity algorithm -- not pre-computed.
--         Draw lines in random shades of color depending on the index.
--         Make a key to swap between rectangular and polar projections.
--         Allow viewpoint to be set with the mouse.
--
--  TODO:  To start with just do brute force visibility by dividing field into cells
--         and doing vis based on center point of cell.
--

import Interface
import Draw
import State
import World
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Lib

main :: IO ()
-- main
--  = do   world           <- initialWorld
--         let state       =  initialState world
        
--         play   FullScreen
--                black 60 state
--                drawState handleInput stepState

drawTest :: Picture -> Game -> Picture
drawTest pict _ = pict

{-# LANGUAGE OverloadedStrings #-}

-- main::IO()
-- main = do
--     map <- (readMaps getTile ["map.txt","map.txt"])
--     let maps = fmap mapToString map
--     print maps

-- data Tile = A | B | C
-- type TileMap = Level

readMaps :: (Char -> Tile) -> [String] -> IO [Level]
readMaps parse filenames = sequence $ map (readMap parse) filenames

readMap :: (Char -> Tile) -> String -> IO [[Tile]]
readMap parse filename = do
    content <- (readFile filename)
    let tilemap = reverseList [[parse sym | sym <- line] | line <- lines content]
    return tilemap

getTile :: Char -> Tile
getTile num
    | num == '0' = Empty
    | num == '1' = Ground
    | num == '2' = Brick
    | num == '3' = BonusBlockActive
    | num == '4' = BonusBlockEmpty
    | otherwise = Empty

-- mapToString :: TileMap -> [[String]]
-- mapToString map = fmap (fmap tileToString) map

-- tileToString :: Tile -> String
-- tileToString A = "A"
-- tileToString B = "B"
-- tileToString C = "C"

reverseList xs = foldl (\x y -> y:x) [] xs 

main = do
    screenResolution <- getScreenSize
    print $ "Screen Resolution: " ++ (show screenResolution)
    marioSprites <- sequence $ map loadBMP (map (\x -> "assets/mario/mario_" ++ x ++ ".bmp") (map show [1..2]))
    envSprites <- sequence $ map loadBMP (map (\x -> "assets/environment/tile_" ++ x ++ ".bmp") (map show [1..4]))
    enemySprites <- sequence $ map loadBMP (map (\x -> "assets/enemies/enemy_" ++ x ++ ".bmp") (map show [1..1]))
    
    maps <- (readMaps getTile ["assets/maps/map_1.txt"])
    
    let assets = Assets marioSprites envSprites enemySprites
    play FullScreen white 60 (initGame maps) (drawGame assets screenResolution) handleGame updateGame