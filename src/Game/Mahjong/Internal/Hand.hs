-- |
-- Module      :  Game.Mahjong.Internal.Hand
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definition of a hand
--   along with hand evaluation functions
module Game.Mahjong.Internal.Hand where

import Data.List (intersperse, sort)
import Game.Mahjong.Internal.Meld
import Game.Mahjong.Internal.Tile


-------------------------------------------------------------------------------

{- Data definition -}

-- | A complete hand when a player has won
data Hand = NoHand
          | Hand { melds    :: [Meld]         -- ^ completed / concealed meld
                 , lastMeld :: Meld           -- ^ the last meld that wins the game
                 , bonusH   :: [Tile Bonus]   -- ^ just a list of bonus tiles
                 }
          | Special { tileSet  :: Tiles         -- ^ the set of onhand tile
                    , lastTile :: WrapTile      -- ^ the last tile obtained
                    , bonusS   :: [Tile Bonus]  -- ^ any bonus tiles
                    }

-- | An inprogress hand during game play, before winning
data InProgress = InProgress { onHand  :: Tiles         -- ^ hidden on hand tiles
                             , melded  :: [Meld]        -- ^ list of revealed meld
                             , bonusIP :: [Tile Bonus]  -- ^ list of revealed bonus tiles
                             }


-------------------------------------------------------------------------------

{- Data instances -}

instance Show Hand where
  show h = case h of
    NoHand          -> "NoHand"
    (Hand m l b)    -> join'' "  " m
           ++ delim ++ show l
           ++ delim ++ joinSort b
    (Special t l b) -> "/" ++ joinSort t ++ "/"
                  ++ delim ++ show l
                  ++ delim ++ joinSort b

instance Show InProgress where
  show (InProgress o m b) = "/" ++ joinSort o ++ "/"
                       ++ delim ++ (join'' "  " m)
                       ++ delim ++ (joinSort b)

join'' :: Show a => String -> [a] -> String
join'' d = concat . intersperse d . map show

joinSort :: (Ord a, Show a) => [a] -> String
joinSort [] = "[]"
joinSort ls = join'' " " . sort $ ls

delim :: String
delim = "  |  "


-------------------------------------------------------------------------------

{- Functions for complete hand -}

noHand :: Hand
noHand                       = NoHand

mkHand :: [Meld] -> Meld -> [Tile Bonus] -> Hand
mkHand                       = Hand

mkSpecial :: Tiles -> Tile a -> [Tile Bonus] -> Hand
mkSpecial ts t tbs           = Special ts (mkWrap t) tbs

getMelds :: Hand -> [Meld]
getMelds (NoHand       )     = []
getMelds (Hand    m l _)     = l : m
getMelds (Special m l _)     = []  -- | TODO: come back to this later

handTiles :: Hand -> Tiles
handTiles (NoHand          ) = []
handTiles (Hand    ms lm bs) = sort (mts ++ bts)
  where mts = concatMap meldTiles (lm : ms)
        bts = map mkWrap bs
handTiles (Special ts lt bs) = sort (ts ++ [lt] ++ bts)
  where bts = map mkWrap bs


-------------------------------------------------------------------------------

{- Functions for in progress hand -}

newInProgress :: InProgress
newInProgress                          = InProgress [] [] []

addTile :: InProgress -> WrapTile -> InProgress
addTile (InProgress oh ms bip) w       = InProgress (sort $ w : oh) ms bip

addMeld :: InProgress -> Meld -> InProgress
addMeld (InProgress oh ms bip) m       = InProgress oh (m : ms) bip

addBonus :: InProgress -> Tile Bonus -> InProgress
addBonus (InProgress oh ms bip) b      = InProgress oh ms (b : bip)

inProgressTiles :: InProgress -> Tiles
inProgressTiles (InProgress oh ms bip) = sort (oh ++ mts ++ bts)
  where mts = concatMap meldTiles ms
        bts = map mkWrap bip

