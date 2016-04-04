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
--  -- ** Utiliy Functions
--  sortPatterns, containsMeld,
--
--  -- ** score functions
--  matchForPatterns
--) where

import Game.Mahjong.Examples
import Game.Mahjong.Hand
import Game.Mahjong.Meld
import Game.Mahjong.Class
import Game.Mahjong.Tile
import Game.Mahjong.Pattern

import Data.List (groupBy, inits, nub, sort, sortBy, tails)
import Data.Ord (comparing)

import Data.Maybe (fromJust)
extract h = (fromJust h, handStat $ fromJust h)

-------------------------------------------------------------------------------
-- Data definition
-------------------------------------------------------------------------------

-- | Newtype for scoring functions
type ScoreFunc = (Hand, HandStat) -> [Pattern]


-------------------------------------------------------------------------------
-- Scoring functions
-------------------------------------------------------------------------------

-- | get a list of patterns the hand satisfies, sorted by highest points desc
matchForPatterns :: Hand -> [Pattern]
matchForPatterns hand =
  if null patterns
  then pure chicken
  else patterns ++ bonusPats
  where
    stat      = (hand, handStat hand)
    patterns  = (<->>) matchers stat
    matchers  = [ matchTrivials         -- 1.  Trivival Patterns
                , matchPungsAndKongs    -- 2.  Pungs & Kongs
                , matchIdenticalSets    -- 3.  Identical Sets
--              , matchSimilarSets      -- 4.  Similar Sets
--              , matchConsecutiveSets  -- 5.  Consecutive Sets
                , matchSuits            -- 6.  Suit Patterns
                , matchTerminals        -- 7.  Terminal Tiles
                , matchHonors           -- 8.  Honor Tiles
                , matchSevenPairs       -- 9.  Seven Pairs
                , matchColors           -- 10. Colors Hands
                , matchIrregular        -- 11. Irregular Hands
                , matchIncidentals      -- 12. Incidental Bonuses
                ]
    bonusPats = matchBonus stat         -- 12. Bonus Tiles


-- | mappend for score functions
--   concats the resulting patterns from each matcher
(<->>) :: [a -> [b]] -> a -> [b]
(<->>) sfs = concat . zipWith id sfs . repeat

-- | sort the patterns by score
sortPatterns :: [Pattern] -> [Pattern]
sortPatterns = reverse . sortBy (comparing score)

-- | Check if the hand contains the given melds
containsMelds :: Hand -> [Meld] -> Bool
containsMelds h = all (\m -> meldElem (getMelds h) m)
  where
    meldElem xs x = any (\m -> meldTileMatch True x m) xs

matchValuePattern :: [Tile] -> [Int] -> Bool
matchValuePattern ts is =
  if isSameTileType ts
  then (sort . fmap tileValue $ ts) == is
  else False

permutingCons :: a -> [a] -> [[a]]
permutingCons x xs = zipWith (\i t -> i ++ [x] ++ t) (inits xs) (tails xs)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

histogram :: Eq a => [a] -> [(a, Int)]
histogram xs = zip keys counts
  where
    keys   = nub xs
    counts = fmap (\x -> count (== x) xs) keys


-------------------------------------------------------------------------------
-- Scoring functions
-------------------------------------------------------------------------------

{- 1.0 Trivial Patterns -}

-- | check if the hand consists of all chows
matchAllChows :: ScoreFunc
matchAllChows (_, hs)
  | numOfChows hs >= 4 = pure allChows
  | otherwise          = []

-- | check for concealed hand
matchConcealedHand :: ScoreFunc
matchConcealedHand (h, _)
  | all isConcealed onHandMelds = pure concealed
  | otherwise                   = []
  where
    onHandMelds = init $ getMelds h

-- | check for self drawing the winning tile
matchSelfDrawn :: ScoreFunc
matchSelfDrawn (h, _)
  | isConcealed lastMeld = pure selfDrawn
  | otherwise            = []
  where
    lastMeld = last $ getMelds h

-- | check for all simple hand
matchAllSimples :: ScoreFunc
matchAllSimples (h, _)
  | all isSimple (getMelds h) = pure allSimples
  | otherwise                 = []

-- | check if the hand consists of all 3 suit types and 2 honor types
matchAllTypes :: ScoreFunc
matchAllTypes (_, hs)
  | all (>= 1) counts = pure allTypes
  | otherwise         = []
  where
    countFuncs = [ numOfCoins, numOfBamboos, numOfCharacters
                 , numOfWinds, numOfDragons
                 ]
    counts     = zipWith id countFuncs $ repeat hs

matchTrivials :: ScoreFunc
matchTrivials = (<->>) [ matchAllChows
                       , matchConcealedHand
                       , matchSelfDrawn
                       , matchAllSimples
                       , matchAllTypes
                       ]

isIllegalCall :: ScoreFunc
isIllegalCall = \_ -> pure illegalCall



{- 2.0 Pungs and Kongs -}

-- 2.1 Pung
matchPungs :: ScoreFunc
matchPungs (_, hs)
  | numOfPungs hs >= 4 = pure allPungs
  | otherwise          = []

-- 2.2 Concealed pungs
matchConcealedPungs :: ScoreFunc
matchConcealedPungs (h, hs)
  | counts == 4 = pure fourConcealedPungs
  | counts == 3 = pure threeConcealedPungs
  | counts == 2 = pure twoConcealedPungs
  | otherwise  = []
  where
    melds  = getMelds h
    counts = count (\m -> isPung m && isConcealed m) melds

-- 2.3 Kongs
matchKongs :: ScoreFunc
matchKongs (_, hs)
  | numOfKongs hs == 4 = pure fourKongs
  | numOfKongs hs == 3 = pure threeKongs
  | numOfKongs hs == 2 = pure twoKongs
  | numOfKongs hs == 1 = pure oneKong
  | otherwise          = []

matchPungsAndKongs :: ScoreFunc
matchPungsAndKongs = (<->>) [ matchPungs
                            , matchConcealedPungs
                            , matchKongs
                            ]


{- 3.0 Identical Sets -}

-- 3.1 Identical sets
-- pungs and kongs shouldn't have value > 1 in groupings,
-- eyes at most 2, and chows up to 4,
-- but should check for isChow just in case.
matchIdenticalSets :: ScoreFunc
matchIdenticalSets (h, _)
  | any (== 4) counts = pure fourIdenticalChows
  | any (== 3) counts = pure threeIdenticalChows
  | twoCount > 1      = pure twoIdenticalChowsTwice
  | twoCount == 1     = pure twoIdenticalChows
  | otherwise         = []
  where
    sorted    = sortBy (comparing meldTiles) $ getMelds h
    groupings = groupBy (\m1 m2 -> meldTiles m1 == meldTiles m2) sorted
    twoOrMore = filter (\g -> length g >= 2 && all isChow g) groupings

    counts    = fmap length twoOrMore
    twoCount  = count (== 2) counts



{- 4.0 Similar Sets -}

-- 4.1 Similar chows
{-
threeSimilarChows :: ScoreFunc
threeSimilarChows = undefined

-- 4.2 Similar pungs
littleThreeSimilarPungs, threeSimilarPung :: ScoreFunc
littleThreeSimilarPungs, threeSimilarPung = undefined
-}
matchSimilarSets :: ScoreFunc
matchSimilarSets = undefined



{- 5.0 Consecutive Sets -}

{-
-- 5.1 Consecutive chows
threeConsecutiveChows, nineTileStraight, threeConsecutiveChowsTwice :: ScoreFunc
threeConsecutiveChows, nineTileStraight, threeConsecutiveChowsTwice = undefined

-- 5.2 Consecutive pungs
threeConsecutivePungs, fourConsecutivePungs, threeMothers :: ScoreFunc
threeConsecutivePungs, fourConsecutivePungs, threeMothers = undefined
-}
matchConsecutiveSets :: ScoreFunc
matchConsecutiveSets = undefined


{- 6.0 Suit Patterns -}

-- 6.1 Mixed and pure
matchOneSuit :: ScoreFunc
matchOneSuit (h, hs)
  | isSameTileType tiles = pure pureOneSuit
  | mixedCond            = pure mixedOneSuit
  | otherwise            = []
  where
    melds      = getMelds h
    tiles      = getHandTiles h
    numCoins   = numOfCoins hs
    numBamboos = numOfBamboos hs
    numChars   = numOfCharacters hs
    numHonors  = numOfWinds hs + numOfDragons hs
    mixedCond  = numHonors >= 1
             && ( (numCoins >= 1 && numBamboos == 0 && numChars == 0)
               || (numCoins == 0 && numBamboos >= 1 && numChars == 0)
               || (numCoins == 0 && numBamboos == 0 && numChars >= 1)
                )

-- 6.2 Nine Gates
-- | only consider pure version
matchNineGates :: ScoreFunc
matchNineGates (h, _)
  | isSpecial h && isOneSuit && matchPattern = pure nineGates
  | otherwise                                = []
  where
    handTiles    = getHandTiles h
    isOneSuit    = isSameTileType handTiles
    matchPattern = matchValuePattern (init handTiles) pat
    pat          = [1, 1] ++ [1..9] ++ [9, 9]

matchSuits :: ScoreFunc
matchSuits = (<->>) [ matchOneSuit
                    , matchNineGates
                    ]


{- 7.0 Terminal Tiles -}

-- 7.1 Chow and pungs
matchTerminals1 :: ScoreFunc
matchTerminals1 (h, _)
  | isSameTileType tiles && matchBM              = pure bigBoundlessMountain
  | isSameTileType tiles && all isTerminal melds = pure littleBoundlessMountain
  | or $ fmap (containsMelds h) pat2             = pure twoTailedTerminals
  | otherwise                                    = accChows ++ accPungs
  where
    melds    = getMelds h
    tiles    = getHandTiles h
    patBM1   = [1, 1, 1, 1, 2, 2, 3, 3, 7, 8, 9, 9, 9, 9]
    patBM2   = [1, 1, 1, 1, 2, 3, 7, 7, 8, 8, 9, 9, 9, 9]
    matchBM  = matchValuePattern tiles patBM1
            || matchValuePattern tiles patBM2

    pat2     = [ [c111, c123, c789, c999]
               , [b111, b123, b789, b999]
               , [k111, k123, k789, k999]
               ]

    patChows  = [ [c123, c789], [b123, b789], [k123, k789] ]
    patPungs  = [ [c111, c999], [b111, b999], [k111, k999] ]
    chowCount = count (== True) . fmap (containsMelds h) $ patChows
    pungCount = count (== True) . fmap (containsMelds h) $ patPungs
    accChows  = take chowCount $ repeat twoTailedTerminalChows
    accPungs  = take pungCount $ repeat twoTailedTerminalPungs

-- 7.2 Mixed and pure
matchTerminals2 :: ScoreFunc
matchTerminals2 (h, hs)
  | all isTerminal tiles             = pure pureGreaterTerminals
  | all isEdge     tiles             = pure mixedGreaterTerminals
  | all isTerminal melds && hasChows = pure pureLesserTerminals
  | all isEdge     melds && hasChows = pure mixedLesserTerminals
  | otherwise                        = []
  where
    melds    = getMelds h
    tiles    = getHandTiles h
    hasChows = numOfChows hs > 0

matchTerminals :: ScoreFunc
matchTerminals = (<->>) [ matchTerminals1
                        , matchTerminals2
                        ]

{- 8.0 Honor Tiles -}

-- 8.1 Winds
matchWinds :: ScoreFunc
matchWinds (h, _)
  | numPungs == 4                 = acc ++ pure bigFourWinds
  | numPungs == 3 && numEyes == 1 = acc ++ pure littleFourWinds
  | numPungs == 3 && numEyes == 0 = acc ++ pure bigThreeWinds
  | numPungs == 2 && numEyes == 1 = acc ++ pure littleThreeWinds
  | otherwise                     = acc
  where
    windMelds = filter isWind $ getMelds h
    numPungs  = count isPung windMelds
    numEyes   = count isEyes windMelds
    acc       = take numPungs $ repeat windPung

-- 8.2 Dragons
matchDragons :: ScoreFunc
matchDragons (h, _)
  | numPungs == 3                 = acc ++ pure bigThreeDragons
  | numPungs == 2 && numEyes == 1 = acc ++ pure littleThreeDragons
  | otherwise                     = acc
  where
    dragonMelds = filter isDragon $ getMelds h
    numPungs    = count isPung dragonMelds
    numEyes     = count isEyes dragonMelds
    acc         = take numPungs $ repeat dragonPung

-- 8.3 Pure honors
matchPureHonors :: ScoreFunc
matchPureHonors (h, _)
  | all isEyes melds && match7Stars = pure allHonorPairs
  | all isHonor melds               = pure allHonors
  | otherwise                       = []
  where
    melds       = getMelds h
    match7Stars = sort (getHandTiles h) == pat
    pat         = honors >>= double
    double      = \x -> [x, x]

matchHonors :: ScoreFunc
matchHonors = (<->>) [ matchWinds
                     , matchDragons
                     , matchPureHonors
                     ]


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

matchColors :: ScoreFunc
matchColors (h, _)
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

matchIncidentals :: ScoreFunc
matchIncidentals (h, _) =
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
    numFlowers = count isFlower . getBonus $ h
    numSeasons = count isSeason . getBonus $ h
    numBonuses = numFlowers + numSeasons

