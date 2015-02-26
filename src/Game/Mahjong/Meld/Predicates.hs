-- |
-- Module      :  Game.Mahjong.Meld.Predicates
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Predicates on melds
module Game.Mahjong.Meld.Predicates
  ( -- * Predicates on meld type ':: Meld -> Bool'
    isChow, isPung, isKong, isEyes

    -- * Predicates on meld based on tile type ':: Meld -> Bool'
  , isCoinM, isBambooM, isCharacterM, isWindM, isDragonM
  , isSimpleM, isTerminalM, isSuitM, isHonorM, isEdgeM
  , isRedM, isGreenM, isBlueM
  ) where

import Game.Mahjong.Internal.Meld

