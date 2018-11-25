{-# OPTIONS_GHC -Wall #-}

module Handle where

import qualified Graphics.Gloss.Interface.Pure.Game as G
import qualified Data.Set as S

import Lib

-- | Update Player speed due to user input.
handleGame :: G.Event -> Game -> Game
handleGame (G.EventKey (G.Char 'w') keyState _ _) = handleKeyPress keyState W_BUTTON
handleGame (G.EventKey (G.Char 'a') keyState _ _) = handleKeyPress keyState A_BUTTON
handleGame (G.EventKey (G.Char 'd') keyState _ _) = handleKeyPress keyState D_BUTTON
handleGame (G.EventKey (G.SpecialKey G.KeyUp) keyState _ _) = handleKeyPress keyState UP_BUTTON
handleGame (G.EventKey (G.SpecialKey G.KeyLeft) keyState _ _) = handleKeyPress keyState LEFT_BUTTON
handleGame (G.EventKey (G.SpecialKey G.KeyRight) keyState _ _) = handleKeyPress keyState RIGHT_BUTTON
handleGame _ = id

handleKeyPress :: G.KeyState -> Movement -> Game -> Game
handleKeyPress keyState mov (Game lvls player state) =
  case keyState of
    G.Up -> Game lvls player (state {pressedKeys = (S.delete mov (pressedKeys state))})
    G.Down -> Game lvls player (state {pressedKeys = (S.insert mov (pressedKeys state))})
