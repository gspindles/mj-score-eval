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


    -- * In progress hand
  , InProgress

    -- ** Funtions on an in progress hand
  , newInProgress
  , addTile, addMeld, addBonus
  , inProgressTiles


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
sp1 = Special [Wrap b1, Wrap b1, Wrap b1, Wrap b2, Wrap b3, Wrap b4, Wrap b5, Wrap b6, Wrap b7, Wrap b8, Wrap b9, Wrap b9, Wrap b9]
              (Wrap b5)
              [f3]

sp2 :: Hand
sp2 = Special [Wrap c1, Wrap c9, Wrap b1, Wrap b9, Wrap k1, Wrap k9, Wrap w1, Wrap w2, Wrap w3, Wrap w4, Wrap d1, Wrap d2, Wrap d3]
              (Wrap c1)
              [f2]

ip :: InProgress
ip = InProgress [Wrap c1, Wrap c3, Wrap b5, Wrap b6, Wrap b7, Wrap d3, Wrap d3]
                [mkPung Revealed w1, mkChow Revealed b3]
                [f3]

