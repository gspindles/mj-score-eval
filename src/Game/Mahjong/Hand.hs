-- |
-- Module      :  Game.Mahjong.Hand
-- Copyright   :  Joseph Ching 2014
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definition of a hand
--   along with hand evaluation functions
module Game.Mahjong.Hand

where

import Game.Mahjong.Meld

{- Data definition -}

data Hand = NoHand
          | Hand
              { getMelds :: [Meld]
              , lastMeld ::  Meld
              , bonusH   ::  Meld
              }
            deriving (Eq, Show)

data InProgress = InProgress
                    { onHand  ::  Meld
                    , melded  :: [Meld]
                    , bonusIP ::  Meld
                    }
                  deriving (Eq, Show)
