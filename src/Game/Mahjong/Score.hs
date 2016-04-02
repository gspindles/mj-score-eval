-- |
-- Module      :  Game.Mahjong.Score
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Functions for scoring mahjong patterns
--   and evaluate score
module Game.Mahjong.Score where

import Game.Mahjong.Examples
import Game.Mahjong.Hand
import Game.Mahjong.Meld
import Game.Mahjong.Class
import Game.Mahjong.Tile
import Game.Mahjong.Pattern

import Data.List (inits, nub, sort, tails)


-------------------------------------------------------------------------------
-- Data definition
-------------------------------------------------------------------------------

-- | Newtype for scoring functions
type ScoreFunc = (Hand, HandStat) -> [Pattern]


-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

matchValuePattern :: [Tile] -> [Int] -> Bool
matchValuePattern ts is =
  if isSameTileType ts
  then (sort . fmap tileValue $ ts) == is
  else False

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


-------------------------------------------------------------------------------
-- Scoring functions
-------------------------------------------------------------------------------

{- 1.0 Trivial Patterns -}

-- | check for chicken hand
isChicken :: ScoreFunc
isChicken = \_ -> pure chicken

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
    f = all isConcealed . getMelds . fst
    p = concealed

-- | check for self drawing the winning tile
isSelfDrawn :: ScoreFunc
isSelfDrawn = scoreHelper f p
  where
    f = isConcealed . last . getMelds . fst
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
isIllegalCall = \_ -> pure illegalCall



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
    2 -> pure twoConcealedPungs
    3 -> pure threeConcealedPungs
    4 -> pure fourConcealedPungs
    _ -> []

concealedHelper :: (Hand, HandStat) -> Int
concealedHelper = length . filter (\x -> isPung x && isConcealed x) . getMelds . fst


-- 2.3 Kongs

-- | check for the number of kongs
isKongs :: ScoreFunc
isKongs p =
  let count = numOfKongs . snd $ p
  in case count of
    1 -> pure oneKong
    2 -> pure twoKongs
    3 -> pure threeKongs
    4 -> pure fourKongs
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
    fs = [ all (\t -> isCoin t      || isHonor t) . getHandTiles . fst
         , all (\t -> isBamboo t    || isHonor t) . getHandTiles . fst
         , all (\t -> isCharacter t || isHonor t) . getHandTiles . fst
         ]
    p  = mixedOneSuit

isPureOneSuit :: ScoreFunc
isPureOneSuit = scoreHelpers fs p
  where
    fs = [ all isCoin      . getHandTiles . fst
         , all isBamboo    . getHandTiles . fst
         , all isCharacter . getHandTiles . fst
         ]
    p  = pureOneSuit


-- 6.2 Nine Gates

-- | only consider pure version
{-
isNineGates :: ScoreFunc
isNineGates ((Special ts lt _), _)
  | map CTile pattern == ts && isCoin      lt = pure nineGates
  | map BTile pattern == ts && isBamboo    lt = pure nineGates
  | map KTile pattern == ts && isCharacter lt = pure nineGates
  | otherwise                                 = []
  where
    pattern = (replicate 2 One) ++ [One .. Nine] ++ (replicate 2 Nine)
isNineGates (_, _) = []

shiftCons :: a -> [a] -> [[a]]
shiftCons x xs = zipWith (\i t -> i ++ [x] ++ t) (inits xs) (tails xs)
-}


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
-}


{- 9.0 Seven Pairs -}

matchSevenPairs :: ScoreFunc
matchSevenPairs (h, hs)
  | isSevenPairs && gcCheck && matchSimple = pure grandChariot
  | isSevenPairs && bfCheck && matchSimple = pure bambooForest
  | isSevenPairs && nnCheck && matchSimple = pure numberNeighborhood
  | isSevenPairs && matchEdge              = pure sevenShiftedPairs
  | isSevenPairs                           = pure sevenPairs
  | otherwise                              = []
  where
    isSevenPairs = numOfEyes       hs == 7
    gcCheck      = numOfCoins      hs == 7
    bfCheck      = numOfBamboos    hs == 7
    nnCheck      = numOfCharacters hs == 7

    handTiles    = getHandTiles h
    matchSimple  = matchValuePattern handTiles pat2
    matchEdge    = matchValuePattern handTiles pat1
                || matchValuePattern handTiles pat3

    pat1   = [1..7] >>= double
    pat2   = [2..8] >>= double
    pat3   = [3..9] >>= double
    double = \x -> [x, x]


{- 10.0 Color Hands -}

matchColor :: ScoreFunc
matchColor (h, _)
  | all isRed   . getHandTiles $ h = pure allRed
  | all isGreen . getHandTiles $ h = pure allGreen
  | otherwise                      = []


{- 11.0 Irregular Hands -}

matchIrregular :: ScoreFunc
matchIrregular (h, _)
  | isSpecial h && isPure   h = pure thirteenOrphansPure
  | isSpecial h && isImpure h = pure thirteenOrphansImpure
  | otherwise   = []
  where
    isPure   = (== edges) . sort . init . getHandTiles
    isImpure = (== edges) . sort . nub  . getHandTiles


{- 12.0 Incidental bonuses -}

matchIncidental :: ScoreFunc
matchIncidental (h, _) =
  case getHandInfo h of
    Nothing -> []
    Just hi ->
      case hi of
        OnFirstDraw       -> pure blessingOfHeaven
        OnFirstDiscard    -> pure blessingOfEarth
        OnSeabed          -> pure finalDraw
        OnRiverbed        -> pure finalDiscard
        OnKongSupplement  -> pure winOnKong
        OnBonusSupplement -> pure winOnBonusTile
        OnKongRobbing     -> pure robbingAKong


{- 13.0 Bonus Tiles -}

matchBonus :: ScoreFunc
matchBonus (h, _)
  | numBonuses == 8 = pure allBonusTiles
  | numFlowers == 4 = pure fourFlowers
  | numSeasons == 4 = pure fourSeasons
  | otherwise       = pure $ updateScore bonusFlowerSeason numBonuses
  where
    numFlowers = length . filter isFlower . getBonus $ h
    numSeasons = length . filter isSeason . getBonus $ h
    numBonuses = numFlowers + numSeasons

