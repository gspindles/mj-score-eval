-- |
-- Module      :  Game.Mahjong.Meld
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Meld module
module Game.Mahjong.Meld 
  ( -- * Meld status
    Status(..)

    -- * Constructors ':: Status -> Tile t -> Meld'
  , mkChow, mkPung, mkKong, mkEyes

    -- * Meld accessors
  , meldType, status, meldTiles

    -- * Predicates for the type of meld ':: Meld -> Bool'
  , isChow, isPung, isKong, isEyes

    -- * Utilities functions
  , shiftMeld
  ) where

import Game.Mahjong.Internal.Meld
