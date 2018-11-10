
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

main = do
    screenResolution <- getScreenSize
    print $ "Screen Resolution: " ++ (show screenResolution)
    marioSprites <- sequence $ map loadBMP (map (\x -> "assets/mario/mario_" ++ x ++ ".bmp") (map show [1..2]))
    envSprites <- sequence $ map loadBMP (map (\x -> "assets/environment/tile_" ++ x ++ ".bmp") (map show [1..4]))
    enemySprites <- sequence $ map loadBMP (map (\x -> "assets/enemies/enemy_" ++ x ++ ".bmp") (map show [1..1]))
    
    let assets = Assets marioSprites envSprites enemySprites
    play FullScreen white 60 initGame (drawGame assets screenResolution) handleGame updateGame