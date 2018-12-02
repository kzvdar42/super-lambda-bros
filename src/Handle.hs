{-# OPTIONS_GHC -Wall #-}

module Handle where

import qualified Graphics.Gloss.Interface.Pure.Game as G
import qualified Data.Set as S

import Lib

-- | Update Player speed due to user input.
handleGame :: G.Event -> Game -> Game
handleGame (G.EventKey (G.Char 'w') keyState _ _) = handleKeyPress keyState P1_U_BUTTON
handleGame (G.EventKey (G.Char 'a') keyState _ _) = handleKeyPress keyState P1_L_BUTTON
handleGame (G.EventKey (G.Char 's') keyState _ _) = handleKeyPress keyState P1_D_BUTTON
handleGame (G.EventKey (G.Char 'd') keyState _ _) = handleKeyPress keyState P1_R_BUTTON
handleGame (G.EventKey (G.SpecialKey G.KeyUp)    keyState _ _) = handleKeyPress keyState P2_U_BUTTON
handleGame (G.EventKey (G.SpecialKey G.KeyLeft)  keyState _ _) = handleKeyPress keyState P2_L_BUTTON
handleGame (G.EventKey (G.SpecialKey G.KeyDown)  keyState _ _) = handleKeyPress keyState P2_D_BUTTON
handleGame (G.EventKey (G.SpecialKey G.KeyRight) keyState _ _) = handleKeyPress keyState P2_R_BUTTON
handleGame (G.EventKey (G.Char 'u') keyState _ _) = handleKeyPress keyState P3_U_BUTTON
handleGame (G.EventKey (G.Char 'h') keyState _ _) = handleKeyPress keyState P3_L_BUTTON
handleGame (G.EventKey (G.Char 'j') keyState _ _) = handleKeyPress keyState P3_D_BUTTON
handleGame (G.EventKey (G.Char 'k') keyState _ _) = handleKeyPress keyState P3_R_BUTTON
handleGame (G.EventKey (G.SpecialKey G.KeyEnter) keyState _ _) = handleKeyPress keyState ENTER_BUTTON
handleGame _ = id

-- | Add/delete the given key to/from the list if key is pressed/unpressed.
handleKeyPress :: G.KeyState -> Movement -> Game -> Game
handleKeyPress keyState mov game =
  case keyState of
    G.Up   -> game {pressedKeys = S.delete mov (pressedKeys game)}
    G.Down -> game {pressedKeys = S.insert mov (pressedKeys game)}
