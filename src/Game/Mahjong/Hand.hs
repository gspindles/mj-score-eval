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
module Game.Mahjong.Hand (
  -- * A completed mahjong hand
  Hand,

  -- ** Constructors
  mkHand, mkSpecial,

  -- ** Hand accessors
  melds, lastMeld, bonus,
  tileSet, lastTile,

  -- ** Functions on a completed hand
  getMelds, handTiles, handTilesWithBonus,


  -- * Stats on a hand
  HandStat(..),

  -- ** Other stats
  numOfSuits, numOfHonors, numOfEdges, numOfMelds,

  -- ** Calculating hand stat
  handStat,


  -- * Examples, TODO: remove later
  h, sp1, sp2
) where

import Game.Mahjong.Class
import Game.Mahjong.Meld
import Game.Mahjong.Tile

import Data.List (intercalate, sort)
import Data.Maybe (fromJust, isJust)


-------------------------------------------------------------------------------
-- Data definition
-------------------------------------------------------------------------------

-- | A completed hand when a player has won
data Hand
  = Hand
    { melds    :: [Meld]  -- ^ completed / concealed meld
    , lastMeld :: Meld    -- ^ the last meld that wins the game
    , bonus    :: [Tile]  -- ^ just a list of bonus tiles
    }
  | Special
    { tileSet  :: [Tile]  -- ^ the set of onhand tile
    , lastTile :: Tile    -- ^ the last tile obtained
    , bonus    :: [Tile]  -- ^ any bonus tiles
    }


-------------------------------------------------------------------------------
-- Typecass instances
-------------------------------------------------------------------------------

instance Show Hand where
  show h =
    case h of
      (Hand m l b)    -> join'' "  " m
             ++ delim ++ show l
             ++ delim ++ joinSort b
      (Special t l b) -> "/" ++ joinSort t ++ "/"
                    ++ delim ++ show l
                    ++ delim ++ joinSort b

join'' :: Show a => String -> [a] -> String
join'' d = intercalate d . map show

joinSort :: (Ord a, Show a) => [a] -> String
joinSort [] = "[]"
joinSort ls = join'' " " . sort $ ls

delim :: String
delim = "  |  "


-------------------------------------------------------------------------------
-- Functions for hand
-------------------------------------------------------------------------------

mkHand :: [Maybe Meld] -> Maybe Meld -> [Tile] -> Maybe Hand
mkHand ms lm tbs =
  if handCheck meldCheck
  then Just $ Hand (fromJust meldList) (fromJust lm) tbs
  else Nothing
  where
    meldList :: Maybe [Meld]
    meldList = sequenceA ms

    meldCheck :: Bool
    meldCheck = isJust meldList
      && (fromJust . fmap (and . fmap (not . isBonus)) $ meldList)

    handCheck :: Bool -> Bool
    handCheck mc = mc
      && allCond [isJust, fromJust . fmap (not . isBonus)] lm
      && all isBonus tbs

mkSpecial :: [Tile] -> Tile -> [Tile] -> Maybe Hand
mkSpecial ts lt tbs
  | specialCheck = Just $ Special ts lt tbs
  | otherwise    = Nothing
  where
    specialCheck ::  Bool
    specialCheck = all (not . isBonus) (lt : ts) && all isBonus tbs

getMelds :: Hand -> [Meld]
getMelds (Hand    m l _) = l : m
getMelds Special{}       = []  -- | TODO: come back to this later

handTiles :: Hand -> [Tile]
handTiles (Hand    ms lm _) = sort $ (lm : ms) >>= meldTiles
handTiles (Special ts lt _) = sort (ts ++ [lt])

handTilesWithBonus :: Hand -> [Tile]
handTilesWithBonus (Hand ms lm bs)    = sort (mts ++ bs)
  where
    mts = (lm : ms) >>= meldTiles
handTilesWithBonus (Special ts lt bs) = sort (ts ++ [lt] ++ bs)

addBonus :: Hand -> Tile -> Hand
addBonus hand b = hand { bonus = b : bonus hand }

-- special hand are not constructed like this
addMeld :: Hand -> Maybe Meld -> Maybe Hand
addMeld h@(Hand ms _ _) m =
  case m of
    Nothing -> Nothing
    Just ts ->
      if not . isBonus $ ts
      then Just $ h { melds = fromJust m : ms }
      else Nothing
addMeld Special{}       _ = Nothing


-------------------------------------------------------------------------------
-- Functions for stats on hand
-------------------------------------------------------------------------------

-- | Stat on a hand
data HandStat =
  HandStat
    { numOfCoins      :: Int   -- ^ The number of coin melds
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

instance Monoid HandStat where
  mempty =
    HandStat
      0 0 0
      0 0
      0 0
      0 0 0 0

  mappend hs1 hs2 =
    HandStat
     (numOfCoins      hs1 + numOfCoins      hs2)
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

-- | number of Suits
numOfSuits :: HandStat -> Int
numOfSuits = sumCond [numOfCoins, numOfBamboos, numOfCharacters]

numOfHonors :: HandStat -> Int
numOfHonors = sumCond [numOfWinds, numOfDragons]

numOfEdges :: HandStat -> Int
numOfEdges = sumCond [numOfTerminals, numOfDragons, numOfWinds]

-- Avoids double counting a kong.
numOfMelds :: HandStat -> Int
numOfMelds = sumCond [numOfChows, numOfPungs, numOfEyes]

handStatStep :: Meld -> HandStat -> HandStat
handStatStep m hs = mappend hs step
  where
    step :: HandStat
    step =
      HandStat
        { numOfCoins      = binary . isCoin      $ m
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


-------------------------------------------------------------------------------
-- Examples for repl
-------------------------------------------------------------------------------

h :: Maybe Hand
h = mkHand [mkChow Revealed c1, mkPung Revealed ws, mkKong Concealed b3, mkEyes Concealed dr]
           (mkChow Revealed k7)
           [f2, s4]

sp1 :: Maybe Hand
sp1 = mkSpecial [b1, b1, b1, b2, b3, b4, b5, b6, b7, b8, b9, b9, b9]
                (b5)
                [f3]

sp2 :: Maybe Hand
sp2 = mkSpecial [c1, c9, b1, b9, k1, k9, we, ws, ww, wn, dr, dg, dw]
                (c1)
                [f2]

