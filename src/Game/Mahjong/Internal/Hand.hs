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
import Data.Monoid
import Game.Mahjong.Internal.Meld
import Game.Mahjong.Internal.Predicate
import Game.Mahjong.Internal.Tile


-------------------------------------------------------------------------------

{- Data definition -}

-- | A complete hand when a player has won
data Hand =
    NoHand
  | Hand    { melds    :: [Meld]        -- ^ completed / concealed meld
            , lastMeld :: Meld          -- ^ the last meld that wins the game
            , bonusH   :: [Tile Bonus]  -- ^ just a list of bonus tiles
            }
  | Special { tileSet  :: Tiles         -- ^ the set of onhand tile
            , lastTile :: WrapTile      -- ^ the last tile obtained
            , bonusS   :: [Tile Bonus]  -- ^ any bonus tiles
            }

-- | An inprogress hand during game play, before winning
data InProgress =
  InProgress { onHand  :: Tiles         -- ^ hidden on hand tiles
             , melded  :: [Meld]        -- ^ list of revealed meld
             , bonusIP :: [Tile Bonus]  -- ^ list of revealed bonus tiles
             }

-- | Stat on a hand
data HandStat =
  HandStat { numOfCoins      :: Int   -- ^ The number of coin melds
           , numOfBamboos    :: Int   -- ^ The number of bamboo melds
           , numOfCharacters :: Int   -- ^ The number of character melds
           , numOfWinds      :: Int   -- ^ The number of wind melds
           , numOfDragons    :: Int   -- ^ The number of dragon melds
           , numOfSimples    :: Int   -- ^ The number of simple melds
           , numOfTerminals  :: Int   -- ^ The number of terminal melds
           , numOfChows      :: Int   -- ^ The number of chows
           , numOfPungs      :: Int   -- ^ The number of pungs
           , numOfKongs      :: Int   -- ^ The number of kongs
           , numOfEyes       :: Int   -- ^ The number of eyes
           } deriving Show

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
join'' d    = concat . intersperse d . map show

joinSort :: (Ord a, Show a) => [a] -> String
joinSort [] = "[]"
joinSort ls = join'' " " . sort $ ls

delim :: String
delim       = "  |  "


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
  where
    mts = concatMap meldTiles (lm : ms)
    bts = map mkWrap bs
handTiles (Special ts lt bs) = sort (ts ++ [lt] ++ bts)
  where
    bts = map mkWrap bs


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
  where
    mts = concatMap meldTiles ms
    bts = map mkWrap bip


-------------------------------------------------------------------------------

{- Functions for stats on hand -}

-- I should use a lens library for all this but w/e idk yolo lol.
-- All of this just so I don't have multiple iterations through a hand's melds.
-- As a punishment for ugliness, I'm not allowed to align those ='s obsessively.

-- On a side note, HandStat should be isomorphic to 11-tuple
instance Monoid HandStat where
  mempty =
    HandStat 0 0 0
             0 0
             0 0
             0 0 0 0

  mappend hs1 hs2 =
    HandStat (numOfCoins      hs1 + numOfCoins      hs2)
             (numOfBamboos    hs1 + numOfBamboos    hs2)
             (numOfCharacters hs1 + numOfCharacters hs2)
             (numOfWinds      hs1 + numOfWinds      hs2)
             (numOfDragons    hs1 + numOfDragons    hs2)
             (numOfSimples    hs1 + numOfSimples    hs2)
             (numOfTerminals  hs1 + numOfTerminals  hs2)
             (numOfChows      hs1 + numOfChows      hs2)
             (numOfPungs      hs1 + numOfPungs      hs2)
             (numOfKongs      hs1 + numOfKongs      hs2)
             (numOfEyes       hs1 + numOfEyes       hs2)

numOfSuits, numOfHonors, numOfEdges, numOfMelds :: HandStat -> Int
numOfSuits  = sum . zipWith id [numOfCoins, numOfBamboos, numOfCharacters] . repeat
numOfHonors = sum . zipWith id [numOfWinds, numOfDragons] . repeat
numOfEdges  = sum . zipWith id [numOfTerminals, numOfDragons, numOfWinds] . repeat
-- Avoid double counting a kong.
numOfMelds  = sum . zipWith id [numOfChows, numOfPungs, numOfEyes] . repeat

handStatStep :: Meld -> HandStat -> HandStat
handStatStep m hs = mappend hs $ step m
  where
    -- Used `toHandStat . map binary . zipWith id [funcs] . repeat` in the past;
    -- however, converting a list of 11 elements over to HandStat was unsightly.
    step :: Meld -> HandStat
    step m =
      HandStat { numOfCoins      = binary . isCoin      $ m
               , numOfBamboos    = binary . isBamboo    $ m
               , numOfCharacters = binary . isCharacter $ m
               , numOfWinds      = binary . isWind      $ m
               , numOfDragons    = binary . isDragon    $ m
               , numOfSimples    = binary . isSimple    $ m
               , numOfTerminals  = binary . isTerminal  $ m
               , numOfChows      = binary . isChow      $ m
               , numOfPungs      = binary . isPung      $ m
               , numOfKongs      = binary . isKong      $ m
               , numOfEyes       = binary . isEyes      $ m
               }

    binary :: Bool -> Int
    binary False = 0
    binary True  = 1

handStat :: Hand -> HandStat
handStat = foldr handStatStep mempty . getMelds

