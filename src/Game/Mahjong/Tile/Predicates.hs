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
  ( -- * Tile type ':: Tile a -> TileType'
    tileType

    -- * Tile predicates ':: Tile a -> Bool'
  , isCoinT, isBambooT, isCharacterT, isWindT, isDragonT, isFlowerT, isSeasonT, isAnimalT
  , isSimpleT, isTerminalT, isSuitT, isHonorT, isEdgeT, isBonusT
  , isRedT, isGreenT, isBlueT
  ) where

import Game.Mahjong.Internal.Tile
import Game.Mahjong.Tile.Collections

