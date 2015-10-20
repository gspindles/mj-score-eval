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

import Data.List (inits, tails)
import Game.Mahjong.Internal.Hand
import Game.Mahjong.Internal.Meld
import Game.Mahjong.Internal.Class
import Game.Mahjong.Internal.Tile
import Game.Mahjong.Pattern


-------------------------------------------------------------------------------

{- Data definition -}

-- | Newtype for scoring functions
type ScoreFunc = (Hand, HandStat) -> [Pattern]

-------------------------------------------------------------------------------

{- Scoring functions -}

scoreHelper :: ((Hand, HandStat) -> Bool) -> Pattern -> ScoreFunc
scoreHelper f p h =
  if f h
  then [p]
  else []

scoreHelpers :: [(Hand, HandStat) -> Bool] -> Pattern -> ScoreFunc
scoreHelpers fs p h =
  if or . zipWith id fs . repeat $ h
  then [p]
  else []

-- | singleton = (:[])
singleton :: a -> [a]
singleton = pure


{- 1.0 Trivial Patterns -}

-- | check for chicken hand
isChicken :: ScoreFunc
isChicken = \_ -> singleton chicken

-- | check if the hand consists of all chows
isAllChows :: ScoreFunc
isAllChows = scoreHelper f p
  where
    f = (>= 4) . numOfChows . snd
    p = allChows

-- | check for concealed hand
isConcealedHand :: ScoreFunc
isConcealedHand = scoreHelper f p
  where
    f = all isConcealed . melds . fst
    p = concealed

-- | check for self drawing the winning tile
isSelfDrawn :: ScoreFunc
isSelfDrawn = scoreHelper f p
  where
    f = isConcealed . lastMeld . fst
    p = selfDrawn

-- | check for all simple hand
isAllSimples :: ScoreFunc
isAllSimples = scoreHelper f p
  where
    f = all isSimple . getMelds . fst
    p = allSimples

-- | check if the hand consists of all 3 suit types and 2 honor types
isAllTypes :: ScoreFunc
isAllTypes = scoreHelper f p
  where
    f  = all (>= 1) . zipWith id ns . repeat . snd
    p  = allTypes
    ns = [numOfCoins, numOfBamboos, numOfCharacters, numOfWinds, numOfDragons]

isIllegalCall :: ScoreFunc
isIllegalCall = \_ -> singleton illegalCall



{- 2.0 Pungs and Kongs -}

-- 2.1 Pung

-- | check if the hand consists of all pungs
isAllPungs :: ScoreFunc
isAllPungs = scoreHelper f p
  where
    f = (>= 4) . numOfPungs . snd
--  f = all isPung . getMelds . fst
    p = allPungs


-- 2.2 Concealed pungs

-- | check for the number of concealed pungs
isConcealedPungs :: ScoreFunc
isConcealedPungs p =
  let count = concealedHelper p
  in case count of
    2 -> singleton twoConcealedPungs
    3 -> singleton threeConcealedPungs
    4 -> singleton fourConcealedPungs
    _ -> []

concealedHelper :: (Hand, HandStat) -> Int
concealedHelper = length . filter (\x -> isPung x && isConcealed x) . getMelds . fst


-- 2.3 Kongs

-- | check for the number of kongs
isKongs :: ScoreFunc
isKongs p =
  let count = numOfKongs . snd $ p
  in case count of
    1 -> singleton oneKong
    2 -> singleton twoKongs
    3 -> singleton threeKongs
    4 -> singleton fourKongs
    _ -> []



{- 3.0 Identical Sets -}
{-
twoIdenticalChows, twoIdenticalChowsTwice, threeIdenticalChows, fourIdenticalChows :: ScoreFunc
twoIdenticalChows, twoIdenticalChowsTwice, threeIdenticalChows, fourIdenticalChows = undefined
-}


{- 4.0 Similar Sets -}

-- 4.1 Similar chows
{-
threeSimilarChows :: ScoreFunc
threeSimilarChows = undefined

-- 4.2 Similar pungs
littleThreeSimilarPungs, threeSimilarPung :: ScoreFunc
littleThreeSimilarPungs, threeSimilarPung = undefined



{- 5.0 Consecutive Sets -}

-- 5.1 Consecutive chows
threeConsecutiveChows, nineTileStraight, threeConsecutiveChowsTwice :: ScoreFunc
threeConsecutiveChows, nineTileStraight, threeConsecutiveChowsTwice = undefined

-- 5.2 Consecutive pungs
threeConsecutivePungs, fourConsecutivePungs, threeMothers :: ScoreFunc
threeConsecutivePungs, fourConsecutivePungs, threeMothers = undefined
-}



{- 6.0 Suit Patterns -}

-- 6.1 Mixed and pure
isMixedOneSuit :: ScoreFunc
isMixedOneSuit = scoreHelpers fs p
  where
    fs = [ all (\t -> isCoin t      || isHonor t) . handTiles . fst
         , all (\t -> isBamboo t    || isHonor t) . handTiles . fst
         , all (\t -> isCharacter t || isHonor t) . handTiles . fst
         ]
    p  = mixedOneSuit

isPureOneSuit :: ScoreFunc
isPureOneSuit = scoreHelpers fs p
  where
    fs = [ all isCoin      . handTiles . fst
         , all isBamboo    . handTiles . fst
         , all isCharacter . handTiles . fst
         ]
    p  = pureOneSuit


-- 6.2 Nine Gates

-- | only consider pure version
isNineGates :: ScoreFunc
isNineGates ((Special ts lt _), _)
  | map CTile pattern == ts && isCoin      lt = singleton nineGates
  | map BTile pattern == ts && isBamboo    lt = singleton nineGates
  | map KTile pattern == ts && isCharacter lt = singleton nineGates
  | otherwise                                 = []
  where
    pattern = (replicate 2 One) ++ [One .. Nine] ++ (replicate 2 Nine)
isNineGates (_, _) = []

shiftCons :: a -> [a] -> [[a]]
shiftCons x xs = zipWith (\i t -> i ++ [x] ++ t) (inits xs) (tails xs)


{- 7.0 Terminal Tiles -}

{-
-- 7.1 Chow and pungs
twoTailedTerminalChows, twoTailedTerminalPungs, twoTailedTerminals, littleBoundlessMountain, bigBoundlessMountain :: ScoreFunc
twoTailedTerminalChows, twoTailedTerminalPungs, twoTailedTerminals, littleBoundlessMountain, bigBoundlessMountain = undefined

-- 7.2 Mixed and pure
mixedLesserTerminals, pureLesserTerminals, mixedGreaterTerminals, pureGreaterTerminals :: ScoreFunc
mixedLesserTerminals, pureLesserTerminals, mixedGreaterTerminals, pureGreaterTerminals = undefined



{- 8.0 Honor Tiles -}

-- 8.1 Dragons
dragonPung, littleThreeDragons, bigThreeDragons :: ScoreFunc
dragonPung, littleThreeDragons, bigThreeDragons = undefined

-- 8.2 Winds
windPung, littleThreeWinds, bigThreeWinds, littleFourWinds, bigFourWinds :: ScoreFunc
windPung, littleThreeWinds, bigThreeWinds, littleFourWinds, bigFourWinds = undefined

-- 8.3 Pure honors
allHonors, allHonorPairs :: ScoreFunc
allHonors, allHonorPairs = undefined



{- 9.0 Seven Pairs -}

-- 9.1 Basic seven pairs
sevenPairs :: ScoreFunc
sevenPairs = undefined

-- 9.2 Specialized seven pairs
sevenShiftedPairs, grandChariot, bambooForest, numberNeighborhood :: ScoreFunc
sevenShiftedPairs, grandChariot, bambooForest, numberNeighborhood = undefined
-}


{- 10.0 Color Hands -}

calculateColor :: ScoreFunc
calculateColor (h, _)
  | all isRed   . handTiles $ h = singleton allRed
  | all isGreen . handTiles $ h = singleton allGreen
  | otherwise                   = []



{- 11.0 Irregular Hands -}

{-
thirteenOrphans :: ScoreFunc
thirteenOrphans = undefined



{- 12.0 Incidental bonuses -}

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
-}



{- 13.0 Bonus Tiles -}

calculateBonus :: ScoreFunc
calculateBonus (h, _)
  | numBonuses == 8 = singleton allBonusTiles
  | numFlowers == 4 = singleton fourFlowers
  | numSeasons == 4 = singleton fourSeasons
  | otherwise       = singleton $ updateScore bonusFlowerSeason numBonuses
  where
    numFlowers = length . filter isFlower . bonus $ h
    numSeasons = length . filter isSeason . bonus $ h
    numBonuses = numFlowers + numSeasons

