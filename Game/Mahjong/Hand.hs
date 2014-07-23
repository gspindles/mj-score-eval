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
module Game.Mahjong.Example

where

import Game.Mahjong.Meld

{- Data definition -}

type Hand = [Meld]
