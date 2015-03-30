-- |
-- Module      :  Game.Mahjong.Tile.Predicates
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Various predicates on tile types
module Game.Mahjong.Tile.Predicates
  ( -- * Suit predicates ':: a -> Bool'
    isCoin, isBamboo, isCharacter
  , isSimple, isTerminal, isSuit

    -- * Honor predicates ':: a -> Bool'
  , isWind, isDragon
  , isHonor, isEdge

    -- * Bonus predicates ':: a -> bool'
  , isFlower, isSeason, isAnimal
  , isBonus

    -- * Color predicates ':: a -> Bool'
  , isRed, isGreen, isBlue
  ) where

import Game.Mahjong.Internal.Meld
import Game.Mahjong.Internal.Predicates
import Game.Mahjong.Internal.Tile

