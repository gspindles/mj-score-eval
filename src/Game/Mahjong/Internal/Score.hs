-- |
-- Module      :  Game.Mahjong.Internal.Score
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definitions and instances of tiles
--   along with tile aliases, collections
--   as well as predicates on tiles
--   and utility functions
module Game.Mahjong.Internal.Score where

import Data.Maybe
import Game.Mahjong.Internal.Hand
import Game.Mahjong.Internal.Meld
import Game.Mahjong.Internal.Predicates
import Game.Mahjong.Pattern


-------------------------------------------------------------------------------

{- Data definition -}

-- | Newtype for scoring functions
type ScoreFunc = (Hand, HandStat) -> Maybe Pattern

-------------------------------------------------------------------------------

{- Scoring functions -}

scoreHelper :: ((Hand, HandStat) -> Bool) -> Pattern -> ScoreFunc
scoreHelper f p h =
  if f h
  then Just p
  else Nothing


-- | 1.0 Trivial Patterns
isChicken :: ScoreFunc
isChicken       = \_ -> Just chicken

isAllChows :: ScoreFunc
isAllChows      = scoreHelper f p
  where
    f  = (>= 4) . numOfChows . snd
--  f  = all isChow . getMelds . fst
    p  = allChows

isConcealedHand :: ScoreFunc
isConcealedHand = scoreHelper f p
  where
    f  = all isConcealed . melds . fst
    p  = concealed

isSelfDrawn :: ScoreFunc
isSelfDrawn     = scoreHelper f p
  where
    f  = isConcealed . lastMeld . fst
    p  = selfDrawn

isAllSimples :: ScoreFunc
isAllSimples    = scoreHelper f p
  where
    f  = all isSimple . getMelds . fst
    p  = allSimples

isAllTypes :: ScoreFunc
isAllTypes      = scoreHelper f p
  where
    f  = all (>= 1) . zipWith id ns . repeat . snd
    p  = allTypes
    ns = [numOfCoins, numOfBamboos, numOfCharacters, numOfWinds, numOfDragons]

isIllegalCall :: ScoreFunc
isIllegalCall   = \_ -> Just illegalCall


-- | 2.0 Pungs and Kongs

-- 2.1 Pung

isAllPungs :: ScoreFunc
isAllPungs = scoreHelper f p
  where
    f = (>= 4) . numOfPungs . snd
--  f = all isPung . getMelds . fst
    p = allPungs

-- 2.2 Concealed pungs

isTwoConcealedPungs, isThreeConcealedPungs, isFourConcealedPungs, isConcealedPungs :: ScoreFunc
isTwoConcealedPungs   = scoreHelper f p
  where
    f = (== 2) . concealedHelper
    p = twoConcealedPungs

isThreeConcealedPungs = scoreHelper f p
  where
    f = (== 3) . concealedHelper
    p = threeConcealedPungs

isFourConcealedPungs  = scoreHelper f p
  where
    f = (== 4) . concealedHelper
    p = fourConcealedPungs

-- Will be using this one, the 3 above are just for completion really.
isConcealedPungs p    =
  let count           = concealedHelper p
  in case count of
    2 -> Just twoConcealedPungs
    3 -> Just threeConcealedPungs
    4 -> Just fourConcealedPungs
    _ -> Nothing

concealedHelper :: (Hand, HandStat) -> Int
concealedHelper       = length . filter (\x -> isPung x && isConcealed x) . getMelds . fst


-- 2.3 Kongs

isOneKong, isTwoKongs, isThreeKongs, isFourKongs, isKongs :: ScoreFunc
isOneKong    = scoreHelper f p
  where
    f = (== 1) . numOfKongs . snd
    p = oneKong

isTwoKongs   = scoreHelper f p
  where
    f = (== 2) . numOfKongs . snd
    p = twoKongs

isThreeKongs = scoreHelper f p
  where
    f = (== 3) . numOfKongs . snd
    p = threeKongs

isFourKongs  = scoreHelper f p
  where
    f = (== 4) . numOfKongs . snd
    p = fourKongs

-- Will be using this one, the 4 above are just for completeness sake.
isKongs p    =
  let count  = numOfKongs . snd $ p
  in case count of
    1 -> Just oneKong
    2 -> Just twoKongs
    3 -> Just threeKongs
    4 -> Just fourKongs
    _ -> Nothing

-- | 3.0 Identical Sets
{-
twoIdenticalChows, twoIdenticalChowsTwice, threeIdenticalChows, fourIdenticalChows :: ScoreFunc
twoIdenticalChows, twoIdenticalChowsTwice, threeIdenticalChows, fourIdenticalChows = undefined
-}

-- | 4.0 Similar Sets

-- 4.1 Similar chows
{-
threeSimilarChows :: ScoreFunc
threeSimilarChows = undefined

-- 4.2 Similar pungs
littleThreeSimilarPungs, threeSimilarPung :: ScoreFunc
littleThreeSimilarPungs, threeSimilarPung = undefined


-- | 5.0 Consecutive Sets 

-- 5.1 Consecutive chows
threeConsecutiveChows, nineTileStraight, threeConsecutiveChowsTwi :: ScoreFunc
threeConsecutiveChows, nineTileStraight, threeConsecutiveChowsTwi = undefined

-- 5.2 Consecutive pungs
threeConsecutivePungs, fourConsecutivePungs, threeMothers :: ScoreFunc
threeConsecutivePungs, fourConsecutivePungs, threeMothers = undefined


-- | 6.0 Suit Patterns

-- 6.1 Mixed and pure
mixedOneSuit, pureOneSuit :: ScoreFunc
mixedOneSuit, pureOneSuit = undefined

-- 6.2 Nine Gates
nineGates :: ScoreFunc
nineGates = undefined


-- | 7.0 Terminal Tiles

-- 7.1 Chow and pungs
twoTailedTerminalChows, twoTailedTerminalPungs, twoTailedTerminals, littleBoundlessMountain, bigBoundlessMountain :: ScoreFunc
twoTailedTerminalChows, twoTailedTerminalPungs, twoTailedTerminals, littleBoundlessMountain, bigBoundlessMountain = undefined

-- 7.2 Mixed and pure
mixedLesserTerminals, pureLesserTerminals, mixedGreaterTerminals, pureGreaterTerminals :: ScoreFunc
mixedLesserTerminals, pureLesserTerminals, mixedGreaterTerminals, pureGreaterTerminals = undefined


-- | 8.0 Honor Tiles

-- 8.1 Dragons
dragonPung, littleThreeDragons, bigThreeDragons :: ScoreFunc
dragonPung, littleThreeDragons, bigThreeDragons = undefined

-- 8.2 Winds
windPung, littleThreeWinds, bigThreeWinds, littleFourWinds, bigFourWinds :: ScoreFunc
windPung, littleThreeWinds, bigThreeWinds, littleFourWinds, bigFourWinds = undefined

-- 8.3 Pure honors
allHonors, allHonorPairs :: ScoreFunc
allHonors, allHonorPairs = undefined


-- | 9.0 Seven Pairs

-- 9.1 Basic seven pairs
sevenPairs :: ScoreFunc
sevenPairs = undefined

-- 9.2 Specialized seven pairs
sevenShiftedPairs, grandChariot, bambooForest, numberNeighborhood :: ScoreFunc
sevenShiftedPairs, grandChariot, bambooForest, numberNeighborhood = undefined


-- | 10.0 Color Hands
allGreen, allRed :: ScoreFunc
allGreen, allRed = undefined


-- | 11.0 Irregular Hands
thirteenOrphans :: ScoreFunc
thirteenOrphans = undefined


-- | 12.0 Incidental bonuses

-- 12.1 Final tile
finalDraw, finalDiscard :: ScoreFunc
finalDraw, finalDiscard = undefined

-- 12.2 Winning on displacement tile
winOnKong, winOnBonusTile :: ScoreFunc
winOnKong, winOnBonusTile = undefined

-- 12.3 Robbing a kong
robbingAKong :: ScoreFunc
robbingAKong = undefined

-- 12.4 Blessings / First tile
blessingOfHeaven, blessingOfEarth :: ScoreFunc
blessingOfHeaven, blessingOfEarth = undefined


-- | 13.0 Bonus Tiles

-- 13.1 Basic flower points
bonusFlowerSeason :: ScoreFunc
bonusFlowerSeason = undefined

-- 13.2 Flower kong
fourFlowers, fourSeasons :: ScoreFunc
fourFlowers, fourSeasons = undefined

-- 12.3 Both sets of bonus tile
allBonusTiles :: ScoreFunc
allBonusTiles = undefined
-}
