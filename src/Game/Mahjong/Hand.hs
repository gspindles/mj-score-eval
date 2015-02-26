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
module Game.Mahjong.Hand where

import Data.List (intersperse, sort)
import Game.Mahjong.Internal.Meld
import Game.Mahjong.Internal.Tile


-------------------------------------------------------------------------------

{- Data definition -}

-- | A complete hand when a player has won
data Hand = NoHand
          | Hand { melds    :: [Meld]  -- ^ completed / concealed meld
                 , lastMeld :: Meld    -- ^ the last meld that wins the game
                 , bonusH   :: Tiles   -- ^ just a list of bonus tiles
                 }

-- | An inprogress hand during game play, before winning
data InProgress = InProgress { onHand  :: Tiles  -- ^ hidden on hand tiles
                             , melded  :: [Meld] -- ^ list of revealed meld
                             , bonusIP :: Tiles  -- ^ list of revealed bonus tiles
                             }


-------------------------------------------------------------------------------

{- Data instances -}

instance Show Hand where
  show NoHand = "NoHand"
  show (Hand m l b) = join'' "  " m
          ++ delim ++ show l
          ++ delim ++ (join'' " " . sort $ b)

instance Show InProgress where
  show (InProgress o m b)  = "/" ++ (join'' " " . sort $ o) ++ "/"
                        ++ delim ++ (join'' "  " m) ++ delim
                                 ++ (join'' " " . sort $ b)

join'' :: Show a => String -> [a] -> String
join'' d = concat . intersperse d . map show

delim :: String
delim = "  |  "


-------------------------------------------------------------------------------

{- Utilities functions -}

getMelds :: Hand -> [Meld]
getMelds (NoHand    ) = []
getMelds (Hand m l _) = l : m


-------------------------------------------------------------------------------

{- Example for repl -}

h :: Hand
h = Hand [mkChow Revealed c1, mkPung Revealed w2, mkKong Concealed b3, mkEyes Concealed d1] (mkChow Revealed k7) [Wrap f2, Wrap s4]

ip :: InProgress
ip = InProgress [Wrap c1, Wrap c3, Wrap b5, Wrap b6, Wrap b7, Wrap d3, Wrap d3] [mkPung Revealed w1, mkChow Revealed b3] [Wrap f3]

