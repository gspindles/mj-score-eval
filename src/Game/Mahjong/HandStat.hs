-- | Stats on a hand.
module Game.Mahjong.HandStat (
  -- * Stats on a hand
  HandStat(..),

  -- ** Other stats
  numOfSuits, numOfHonors, numOfEdges, numOfMelds,

  -- ** Calculating hand stat
  handStat,
) where

import Game.Mahjong.Class
import Game.Mahjong.Hand
import Game.Mahjong.Meld
import Game.Mahjong.Tile


-------------------------------------------------------------------------------
-- Stats on a hand
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
    , numOfSequences  :: Int   -- ^ The number of sequences
    , numOfTriplets   :: Int   -- ^ The number of triplets
    , numOfQuartets   :: Int   -- ^ The number of quartets
    , numOfPairs      :: Int   -- ^ The number of pairs of eyes
    } deriving (Eq, Show)

instance Semigroup HandStat where
  hs1 <> hs2 =
    HandStat
     (numOfCoins      hs1 + numOfCoins      hs2)
     (numOfBamboos    hs1 + numOfBamboos    hs2)
     (numOfCharacters hs1 + numOfCharacters hs2)
     (numOfWinds      hs1 + numOfWinds      hs2)
     (numOfDragons    hs1 + numOfDragons    hs2)
     (numOfSimples    hs1 + numOfSimples    hs2)
     (numOfTerminals  hs1 + numOfTerminals  hs2)
     (numOfSequences  hs1 + numOfSequences  hs2)
     (numOfTriplets   hs1 + numOfTriplets   hs2)
     (numOfQuartets   hs1 + numOfQuartets   hs2)
     (numOfPairs      hs1 + numOfPairs      hs2)

instance Monoid HandStat where
  mempty =
    HandStat
      0 0 0
      0 0
      0 0
      0 0 0 0

-- | number of Suits
numOfSuits :: HandStat -> Int
numOfSuits = sumCond [numOfCoins, numOfBamboos, numOfCharacters]

numOfHonors :: HandStat -> Int
numOfHonors = sumCond [numOfWinds, numOfDragons]

numOfEdges :: HandStat -> Int
numOfEdges = sumCond [numOfTerminals, numOfDragons, numOfWinds]

-- Avoids double counting a quartet.
numOfMelds :: HandStat -> Int
numOfMelds = sumCond [numOfSequences, numOfTriplets, numOfPairs]

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
        , numOfSequences  = binary . isSequence  $ m
        , numOfTriplets   = binary . isTriplet   $ m
        , numOfQuartets   = binary . isQuartet   $ m
        , numOfPairs      = binary . isPair      $ m
        }

    binary :: Bool -> Int
    binary False = 0
    binary True  = 1

handStat :: Hand -> HandStat
handStat = foldr handStatStep mempty . getMelds

