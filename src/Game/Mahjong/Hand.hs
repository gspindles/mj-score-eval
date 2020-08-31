{-# Language LambdaCase #-}

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
  addMeld, addBonus, addHandInfo,


  -- * Stats on a hand
  HandStat(..),

  -- ** Other stats
  numOfSuits, numOfHonors, numOfEdges, numOfMelds,

  -- ** Calculating hand stat
  handStat,
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
  | OnQuartetSupplement
  | OnBonusSupplement
  | OnQuartetRobbing
    deriving (Eq, Show)

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
  pp = \case
    (Hand m b i)      -> joinPP "  " m
                      ++ delim ++ joinSort b
                      ++ info i
    (Special t l b i) -> "-/" ++ joinSort t ++ "/"
                      ++ delim ++ pp l
                      ++ delim ++ joinSort b
                      ++ info i
    where
      info (Just hi)  = delim ++ show hi
      info Nothing    = ""

      delim           = "  |  "

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
addHandInfo (Hand ms bts _)       nhi = Hand ms bts (Just nhi)
addHandInfo (Special ts lt bts _) nhi = Special ts lt bts (Just nhi)


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
    , numOfSequences  :: Int   -- ^ The number of sequences
    , numOfTriplets   :: Int   -- ^ The number of triplets
    , numOfQuartets   :: Int   -- ^ The number of quartets
    , numOfPairs      :: Int   -- ^ The number of pairs of eyes
    } deriving Show

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

