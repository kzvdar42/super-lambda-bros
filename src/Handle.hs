{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE PatternGuards #-}

module Handle where

import qualified Graphics.Gloss.Interface.Pure.Game as G
import qualified Data.Set as S

import Lib

-- | Update Player speed due to user input.
handleGame :: G.Event -> Game -> Game
handleGame (G.EventKey key keyState _ _) (Game lvls player state)
  | G.SpecialKey G.KeyUp <- key
  , G.Down               <- keyState
    = Game lvls player (state {pressedKeys = (S.insert UP_BUTTON (pressedKeys state))})
handleGame (G.EventKey key keyState _ _) (Game lvls player state)
  | G.SpecialKey G.KeyUp <- key
  , G.Up                 <- keyState
    = Game lvls player (state {pressedKeys = (S.delete UP_BUTTON (pressedKeys state))})
handleGame (G.EventKey key keyState _ _) (Game lvls player state)
  | G.SpecialKey G.KeyLeft <- key
  , G.Down                 <- keyState
    = Game lvls player (state {pressedKeys = (S.insert LEFT_BUTTON (pressedKeys state))})
handleGame (G.EventKey key keyState _ _) (Game lvls player state)
  | G.SpecialKey G.KeyLeft <- key
  , G.Up                   <- keyState
    = Game lvls player (state {pressedKeys = (S.delete LEFT_BUTTON (pressedKeys state))})
handleGame (G.EventKey key keyState _ _) (Game lvls player state)
  | G.SpecialKey G.KeyRight <- key
  , G.Down                  <- keyState
    = Game lvls player (state {pressedKeys = (S.insert RIGHT_BUTTON (pressedKeys state))})
handleGame (G.EventKey key keyState _ _) (Game lvls player state)
  | G.SpecialKey G.KeyRight <- key
  , G.Up                    <- keyState
    = Game lvls player (state {pressedKeys = (S.delete RIGHT_BUTTON (pressedKeys state))})
handleGame _ game  = game
