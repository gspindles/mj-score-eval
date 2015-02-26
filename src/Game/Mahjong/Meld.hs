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
  ( -- * Constructors ':: Status -> Tile t -> Meld'
    mkChow, mkPung, mkKong, mkEyes

    -- * Predicates for the type of meld ':: Meld -> Bool'
  , isChow, isPung, isKong, isEyes
  
    -- * predicate for meld base on tile type ':: Meld -> Bool'
  , isCoinM, isBambooM, isCharacterM, isWindM, isDragonM
  , isSimpleM, isTerminalM, isSuitM, isHonorM, isEdgeM
  , isRedM, isGreenM, isBlueM
  ) where

import Game.Mahjong.Internal.Meld
