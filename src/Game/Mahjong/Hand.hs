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
  , noHand, mkHand, mkSpecial

    -- ** Hand accessors
  , melds, lastMeld, bonusH
  , tileSet, lastTile, bonusS

    -- ** Functions on a completed hand
  , getMelds, handTiles


    -- * Stats on a hand
  , HandStat(..)

    -- ** Other stats
  , numOfSuits, numOfHonors, numOfEdges, numOfMelds

    -- ** Calculating hand stat
  , handStatStep, handStat
  ) where

import Game.Mahjong.Internal.Hand
import Game.Mahjong.Internal.Meld
import Game.Mahjong.Internal.Tile


-------------------------------------------------------------------------------

{- Example for repl -}

h :: Hand
h = Hand [mkChow Revealed c1, mkPung Revealed w2, mkKong Concealed b3, mkEyes Concealed d1]
         (mkChow Revealed k7)
         [f2, s4]

sp1 :: Hand
sp1 = Special [b1, b1, b1, b2, b3, b4, b5, b6, b7, b8, b9, b9, b9]
              (b5)
              [f3]

sp2 :: Hand
sp2 = Special [c1, c9, b1, b9, k1, k9, w1, w2, w3, w4, d1, d2, d3]
              (c1)
              [f2]

