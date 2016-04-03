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
  HandInfo(..), Hand,

  -- ** Constructors
  mkHand, mkSpecial,
  mkHand1, mkSpecial1,

  -- ** Functions on a completed hand
  isSpecial,
  getMelds, getHandTiles, getBonus, getHandInfo, hasHandInfo,

  -- ** update functions
  addBonus, addHandInfo,


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

import Data.List (nub, sort)
import Data.Maybe (fromJust, isJust)


-------------------------------------------------------------------------------
-- Data definition
-------------------------------------------------------------------------------

-- | Hand information.
--   These are for hands that satisfies special requirements
--   rather than matching specific patterns.
data HandInfo
  = OnFirstDraw
  | OnFirstDiscard
  | OnSeabed
  | OnRiverbed
  | OnKongSupplement
  | OnBonusSupplement
  | OnKongRobbing
    deriving (Eq, Ord, Show)

-- | A completed hand when a player has won
data Hand
  = Hand [Meld] [Tile] (Maybe HandInfo)
    -- Takes a list of meld, a list of bonus tiles
    -- and maybe additional hand info.
  | Special [Tile] Tile [Tile] (Maybe HandInfo)
    -- Takes the set of onhand tile, the last tile,
    -- a list of bonus tiles, and maybe additional hand info.
  deriving (Eq, Show)


-------------------------------------------------------------------------------
-- Typecass instances
-------------------------------------------------------------------------------

instance Pretty Hand where
  pp h =
    case h of
      (Hand m b i)      -> joinPP "  " m
                       ++ delim ++ joinSort b
                       ++ info i
      (Special t l b i) -> "-/" ++ joinSort t ++ "/"
                       ++ delim ++ pp l
                       ++ delim ++ joinSort b
                       ++ info i
    where
      info i =
        case i of
          Just hi -> delim ++ show hi
          Nothing -> ""

      delim = "  |  "



joinSort :: (Ord a, Pretty a) => [a] -> String
joinSort [] = "[]"
joinSort ls = joinPP " " . sort $ ls


-------------------------------------------------------------------------------
-- Functions for hand
-------------------------------------------------------------------------------

mkHand :: [Meld] -> [Tile] -> Maybe HandInfo -> Hand
mkHand = Hand

mkSpecial :: [Tile] -> Tile -> [Tile] -> Maybe HandInfo -> Hand
mkSpecial = Special

-- | This is the safe version that does validation
mkHand1 :: [Maybe Meld] -> [Tile] -> Maybe HandInfo -> Maybe Hand
mkHand1 ms bts hi =
  if all isJust ms && all isBonus bts
  then Just $ Hand (fromJust $ sequenceA ms) bts hi
  else Nothing

-- | This is the safe version that does validation
mkSpecial1 :: [Tile] -> Tile -> [Tile] -> Maybe HandInfo -> Maybe Hand
mkSpecial1 ts lt bts hi
  | specialCheck = Just $ Special ts lt bts hi
  | otherwise    = Nothing
  where
    specialCheck :: Bool
    specialCheck = all (not . isBonus) (lt : ts)
                && all isBonus bts

isSpecial :: Hand -> Bool
isSpecial (Hand _ _ _)      = False
isSpecial (Special _ _ _ _) = True

getMelds :: Hand -> [Meld]
getMelds (Hand ms _ _) = ms
getMelds Special{}   = []  -- | TODO: come back to this later

getHandTiles :: Hand -> [Tile]
getHandTiles (Hand    ms _ _)    = ms >>= meldTiles
getHandTiles (Special ts lt _ _) = (ts ++ [lt])

getBonus :: Hand -> [Tile]
getBonus (Hand _ bts _)      = bts
getBonus (Special _ _ bts _) = bts

getHandInfo :: Hand -> Maybe HandInfo
getHandInfo (Hand _ _ hi)      = hi
getHandInfo (Special _ _ _ hi) = hi

hasHandInfo :: Hand -> Bool
hasHandInfo = (== Nothing) . getHandInfo

-- special hand are not constructed like this
-- new melds are added at the end
addMeld :: Hand -> Maybe Meld -> Maybe Hand
addMeld (Hand ms bts hi) m =
  case m of
    Nothing -> Nothing
    Just _  -> Just $ Hand (ms ++ pure (fromJust m)) bts hi
addMeld Special{}     _    = Nothing

-- Add bonus tile to a hand
addBonus :: Hand -> Tile -> Hand
addBonus (Hand ms bts hi) b       = Hand ms (nub $ b : bts) hi
addBonus (Special ts lt bts hi) b = Special ts lt (nub $ b : bts) hi

-- Add additional information about winning conditions to a hand
addHandInfo :: Hand -> HandInfo -> Hand
addHandInfo h@(Hand ms bts hi)       nhi =
  case hi of
    Nothing -> Hand ms bts (Just nhi)
    Just _  -> h
addHandInfo s@(Special ts lt bts hi) nhi =
  case hi of
    Nothing -> Special ts lt bts (Just nhi)
    Just _  -> s


-------------------------------------------------------------------------------
-- Functions for stats on hand
-------------------------------------------------------------------------------

-- | Stat on a hand
data HandStat =
  HandStat {
      numOfCoins      :: Int   -- ^ The number of coin melds
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
h = mkHand1 [
    mkChow Revealed c1
  , mkPung Revealed ws
  , mkKong Concealed b3
  , mkEyes Concealed dr
  , mkChow Revealed k7
  ]
  [f2, s4]
  Nothing

sp1 :: Maybe Hand
sp1 = mkSpecial1 [b1, b1, b1, b2, b3, b4, b5, b6, b7, b8, b9, b9, b9]
                b5
                [f3]
                (Just OnSeabed)

sp2 :: Maybe Hand
sp2 = mkSpecial1 [c1, c9, b1, b9, k1, k9, we, ws, ww, wn, dr, dg, dw]
                c1
                [f2]
                (Just OnRiverbed)

