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
import Game.Mahjong.Internal.Predicate
import Game.Mahjong.Pattern


-------------------------------------------------------------------------------

{- Data definition -}

-- | Newtype for scoring functions
newType ScoreFunc = Hand -> Maybe Pattern

-------------------------------------------------------------------------------

{- Scoring functions -}

-- | 1.0 Trivial Patterns
isChicken :: ScoreFunc
isChicken = \_ -> Just chicken

isAllChows :: ScoreFunc
isAllChows h
  | (>=) 4 . filter isChow . getMelds $ h = Just allChows
  | otherwise                             = Nothing

isConcealed :: ScoreFunc
isConcealed h
  | all . map isConcealed . melds $ h = Just concealed
  | otherwise                         = Nothing

selfDrawn, allSimples, allTypes, illegalCall :: ScoreFunc
chicken, allChows, concealed, selfDrawn, allSimples, allTypes, illegalCall = undefined


-- | 2.0 Pungs and Kongs

-- 2.1 Pung
allPungs :: ScoreFunc
allPungs = undefined

-- 2.2 Concealed pungs
twoConcealedPungs, threeConcealedPungs, fourConcealedPungs :: ScoreFunc
twoConcealedPungs, threeConcealedPungs, fourConcealedPungs = undefined

-- 2.3 Kongs
oneKong, twoKongs, threeKongs, fourKongs :: ScoreFunc
oneKong, twoKongs, threeKongs, fourKongs = undefined


-- | 3.0 Identical Sets
twoIdenticalChows, twoIdenticalChowsTwice, threeIdenticalChows, fourIdenticalChows :: ScoreFunc
twoIdenticalChows, twoIdenticalChowsTwice, threeIdenticalChows, fourIdenticalChows = undefined


-- | 4.0 Similar Sets

-- 4.1 Similar chows
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
