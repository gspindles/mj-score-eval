-- |
-- Module      :  Game.Mahjong.Hand
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definition of a hand
--   along with hand evaluation functions
module Game.Mahjong.Hand 
  ( -- * Completed hand
    Hand

    -- ** constructors
  , noHand, mkHand

    -- ** Hand accessors
  , melds, lastMeld, bonusH

    -- ** Functions on a completed hand
  , getMelds, handTiles


    -- * In progress hand
  , InProgress

    -- ** Funtions on an in progress hand
  , newInProgress
  , addTile, addMeld, addBonus
  , inProgressTiles


  ) where

import Game.Mahjong.Internal.Hand
import Game.Mahjong.Internal.Meld
import Game.Mahjong.Internal.Tile


-------------------------------------------------------------------------------

{- Example for repl -}

h :: Hand
h = Hand [mkChow Revealed c1, mkPung Revealed w2, mkKong Concealed b3, mkEyes Concealed d1] (mkChow Revealed k7) [f2, s4]

ip :: InProgress
ip = InProgress [Wrap c1, Wrap c3, Wrap b5, Wrap b6, Wrap b7, Wrap d3, Wrap d3] [mkPung Revealed w1, mkChow Revealed b3] [f3]

