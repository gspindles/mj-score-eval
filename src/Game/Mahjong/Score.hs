{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Functions for scoring mahjong patterns
--   and evaluate score
module Game.Mahjong.Score (
    -- ** score functions
    scoreHand, calculateScore, matchForPatterns, matchBonus
) where

import Game.Mahjong.Hand
import Game.Mahjong.Meld
import Game.Mahjong.Class
import Game.Mahjong.Tile
import Game.Mahjong.Pattern
import Game.Mahjong.Static.Tiles
import Game.Mahjong.Static.Melds

import Control.Arrow ((&&&))
import Control.Applicative (liftA2)
import Data.Foldable (maximumBy)
import Data.List (group, groupBy, inits, intersect, nub, sort, sortBy, tails)
import Data.Maybe (fromJust)
import Data.Monoid (Any(..), Sum(..))
import Data.Ord (comparing)


-------------------------------------------------------------------------------
-- Data definition
-------------------------------------------------------------------------------

type ScoreResults = (Int, [Pattern])

instance Pretty ScoreResults where
  pp (total, patterns) = "Total: " ++ show total ++ "\n"
                      ++ "Patterns:\n"
                      ++ concatMap (\p -> "  " ++ pp p ++ "\n") patterns

-- | Newtype for scoring functions
type ScoreFunc = (Hand, HandStat) -> [Pattern]


-------------------------------------------------------------------------------
-- Scoring functions
-------------------------------------------------------------------------------

-- | Calculates the scoring results of the hand
scoreHand :: Maybe Hand -> ScoreResults
scoreHand Nothing  = score &&& pure $ illegalCall
scoreHand (Just h) = calculateScore $ matchForPatterns h

-- | Calculate the total results among the patterns
calculateScore :: [Pattern] -> ScoreResults
calculateScore ps
  | elem illegalCall ps   = score &&& pure $ illegalCall
  | not (null limitHands) = (limitScore, limitHands)
  | otherwise             = (handScore, ps)
  where
    limitHands = filter ((>= 320) . score) ps
    limitScore = score $ maximumBy (comparing score) limitHands

    scoreCap   = 320
    handTotal  = getSum $ foldMap (Sum . score) ps
    handScore  = if handTotal >= scoreCap
                 then scoreCap
                 else handTotal

-- | get a list of patterns the hand satisfies, sorted by highest points desc
matchForPatterns :: Hand -> [Pattern]
matchForPatterns hand =
  if null patterns
  then pure chickenHand        ++ bonusPats
  else sortPatterns $ patterns ++ bonusPats
  where
    stat      = (hand, handStat hand)
    patterns  | isSpecial hand = matchSpecial stat
              | otherwise      = distribute matchers stat
    matchers  = [ matchTrivials            -- 1.  Trivival Patterns
                , matchTripletsAndQuartets -- 2.  Triplets & Quartets
                , matchIdenticalSets       -- 3.  Identical Sets
                , matchSimilarSets         -- 4.  Similar Sets
                , matchConsecutiveSets     -- 5.  Consecutive Sets
                , matchSuits               -- 6.  Suit Patterns
                , matchTerminals           -- 7.  Terminal Tiles
                , matchHonors              -- 8.  Honor Tiles
                , matchColors              -- 9. Colors Hands
                , matchSevenPairs          -- 10. Irregular Hands - seven pairs
                , matchIncidentals         -- 11. Incidental Bonuses
                ]
    bonusPats = matchBonus stat            -- 12. Bonus Tiles


-- | mappend for score functions
--   concats the resulting patterns from each matcher
distribute :: [a -> [b]] -> a -> [b]
distribute sfs = concat . zipWith id sfs . repeat

-- | sort the patterns by score
sortPatterns :: [Pattern] -> [Pattern]
sortPatterns = reverse . sortBy (comparing score)

-- | Check if the hand contains the given melds
containsMelds :: Hand -> [Meld] -> Bool
containsMelds h = all (\m -> meldElem (getMelds h) m)
  where
    meldElem xs x = any (\m -> meldTileMatch True x m) xs

matchValuePattern :: [Tile] -> [Int] -> Bool
matchValuePattern ts is = isSameTileType ts
                       && (sort . fmap tileValue $ ts) == is

permutingCons :: a -> [a] -> [[a]]
permutingCons _ [] = []
permutingCons x xs = zipWith (\i t -> i ++ [x] ++ t) (inits xs) (tails xs)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

histogram :: Ord a => [a] -> [(a, Int)]
histogram [] = []
histogram xs = fmap (head &&& length) . group . sort $ xs

commonElems :: (Eq a, Applicative f, Foldable t) => t (f [a]) -> f [a]
commonElems = foldr1 (liftA2 intersect)

filterAndGroupByTileType :: (Meld -> Bool) -> [Meld] -> [[[Int]]]
filterAndGroupByTileType f = projected
  where
    tt        = tileType . head . meldTiles
    sorted    = sortBy (comparing tt) . filter f
    groupings = groupBy (\m1 m2 -> tt m1 == tt m2) . sorted
    projected = fmap (fmap (fmap tileValue . meldTiles)) . groupings


-------------------------------------------------------------------------------
-- Scoring functions
-------------------------------------------------------------------------------

{- 1.0 Trivial Patterns -}

-- | check if the hand consists of all sequences
matchAllSequences :: ScoreFunc
matchAllSequences (_, hs)
  | numOfSequences hs >= 4 = pure allSequences
  | otherwise              = []

-- | check for concealed hand
matchConcealedHand :: ScoreFunc
matchConcealedHand (h, _)
  | all isConcealed onHandMelds = pure concealedHand
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
matchTrivials = distribute
  [ matchAllSequences , matchConcealedHand, matchSelfDrawn, matchAllSimples, matchAllTypes ]



{- 2.0 Triplets and Quartets -}

-- 2.1 Triplet
matchTriplets :: ScoreFunc
matchTriplets (_, hs)
  | numOfTriplets hs >= 4 = pure allTriplets
  | otherwise             = []

-- 2.2 Concealed triplets
matchConcealedTriplets :: ScoreFunc
matchConcealedTriplets (h, _)
  | counts == 4 = pure fourConcealedTriplets
  | counts == 3 = pure threeConcealedTriplets
  | counts == 2 = pure twoConcealedTriplets
  | otherwise  = []
  where
    melds  = getMelds h
    counts = count (\m -> isTriplet m && isConcealed m) melds

-- 2.3 Quartets
matchQuartets :: ScoreFunc
matchQuartets (_, hs)
  | p == 4    = pure fourQuartets
  | p == 3    = pure threeQuartets
  | p == 2    = pure twoQuartets
  | p == 1    = pure oneQuartet
  | otherwise = []
  where
    p = numOfQuartets hs

matchTripletsAndQuartets :: ScoreFunc
matchTripletsAndQuartets = distribute
  [ matchTriplets, matchConcealedTriplets, matchQuartets ]



{- 3.0 Identical Sets -}

-- 3.1 Identical sets
-- triplets and quartets shouldn't have value > 1 in groupings,
-- eyes at most 2, and sequences up to 4,
-- but should check for isSequence just in case.
matchIdenticalSets :: ScoreFunc
matchIdenticalSets (h, _)
  | any (== 4) counts = pure fourIdenticalSequences
  | any (== 3) counts = pure threeIdenticalSequences
  | twoCount > 1      = pure twoIdenticalSequencesTwice
  | twoCount == 1     = pure twoIdenticalSequences
  | otherwise         = []
  where
    sorted    = sortBy (comparing meldTiles) $ getMelds h
    groupings = groupBy (\m1 m2 -> meldTiles m1 == meldTiles m2) sorted
    twoOrMore = filter (\g -> length g >= 2 && all isSequence g) groupings

    counts    = length <$> twoOrMore
    twoCount  = count (== 2) counts



{- 4.0 Similar Sets -}

-- 4.1 Similar sequences
-- 4.2 Similar triplets
matchSimilarSets :: ScoreFunc
matchSimilarSets (h, _)
  | similarCheck intersectionSequence  = pure threeSimilarSequences
  | similarCheck intersectionTriplet   = pure threeSimilarTriplets
  | length eye == 1 && isLTSP          = pure littleThreeSimilarTriplets
  | otherwise                          = []
  where
    melds                = getMelds h
    similarCheck         = getAny . foldMap (Any . not . null)

    checkSequence        = \m -> isSequence m && isSuit m
    projectedSequence    = filterAndGroupByTileType checkSequence melds
    intersectionSequence = if length projectedSequence == 3
                           then commonElems projectedSequence
                           else []

    checkTriplet         = \m -> isTriplet m && isSuit m
    projectedTriplet     = filterAndGroupByTileType checkTriplet melds
    intersectionTriplet  = if length projectedTriplet == 3
                           then commonElems projectedTriplet
                           else []

    eye                  = filter (\m -> isPair m && isSuit m) melds
    eyeTile              = head . meldTiles . head $ eye
    eyeTileValue         = toEnum @Values $ (tileValue eyeTile) - 1
    getOtherTileType tt
      | tt == Coin       = [ mkBamboo, mkCharacter ]
      | tt == Bamboo     = [ mkCoin  , mkCharacter ]
      | tt == Character  = [ mkCoin  , mkBamboo    ]
      | otherwise        = []  -- shouldn't get here due to length eye == 1 check above
    otherTwoTiles        = fmap ($ eyeTileValue) . getOtherTileType $ tileType eyeTile
    otherTwoTriplets     = fmap (fromJust . mkTriplet Concealed . take 3 . repeat) otherTwoTiles
    isLTSP               = containsMelds h otherTwoTriplets



{- 5.0 Consecutive Sets -}

-- 5.1 Consecutive sequences
matchConsecutiveSequences :: ScoreFunc
matchConsecutiveSequences (h, _)
  | length sorted == 4 && fourConsSCheck   = pure fourConsecutiveSequences
  | length sorted == 4 && threeConsSTCheck = pure threeConsecutiveSequencesTwice
  | length sorted >= 3 && nineTSCheck      = pure nineTileStraight
  | length sorted >= 3 && threeConsSCheck  = pure threeConsecutiveSequences
  | otherwise                              = []
  where
    melds            = getMelds h
    sequences        = filter (\m -> isSequence m && isSuit m) melds
    sorted           = sortBy (comparing meldTiles) sequences
    fstSequence      = head sorted
    sndSequence      = head $ tail sorted

    build4ConsS1     = take 4 . iterate next
    build4ConsS2     = take 4 . iterate (next . next)
    build3ConsST1 m  = [ m
                       , next m
                       , next (next m)
                       , next (next (next (next m)))
                       ]
    build3ConsST2 m  = [ m
                       , next (next m)
                       , next (next (next m))
                       , next (next (next (next m)))
                       ]
    build3ConsS1     = take 3 . iterate next
    build3ConsS2     = take 3 . iterate (next . next)
    build3ConsS3     = take 3 . iterate (next . next . next)
    sequence123s m   = any (meldTileMatch True m) [ c123, b123, k123 ]

    fourConsSCheck   = ( sequence123s fstSequence
                    && containsMelds h (build4ConsS2 fstSequence) )
                    || containsMelds h (build4ConsS1 fstSequence)
    threeConsSTCheck = containsMelds h (build3ConsST1 fstSequence)
                    || containsMelds h (build3ConsST2 fstSequence)
    nineTSCheck      = ( sequence123s fstSequence
                    && containsMelds h (build3ConsS3 fstSequence) )
                    || ( sequence123s sndSequence
                    && containsMelds h (build3ConsS2 sndSequence) )
    threeConsSCheck  = containsMelds h (build3ConsS1 fstSequence)
                    || containsMelds h (build3ConsS2 fstSequence)
                    || containsMelds h (build3ConsS1 sndSequence)
                    || containsMelds h (build3ConsS2 sndSequence)

-- 5.2 Consecutive triplets
matchConsecutiveTriplets :: ScoreFunc
matchConsecutiveTriplets (h, _)
  | length sorted == 3 && threeMCheck     = pure threeMothers
  | length sorted == 4 && fourConsTCheck  = pure fourConsecutiveTriplets
  | length sorted >= 3 && threeConsTCheck = pure threeConsecutiveTriplets
  | otherwise                             = []
  where
    melds           = getMelds h
    triplets        = filter (\m -> isTriplet m && isSuit m) melds
    sorted          = sortBy (comparing meldTiles) triplets
    fstTriplet      = head sorted
    sndTriplet      = head $ tail sorted
    sonTiles        = fmap (head . meldTiles) sorted

    build3ConsT     = take 3 . iterate next
    build4ConsT     = take 4 . iterate next
    sonSequence     = mkSequence Revealed sonTiles
    hasSon (Just c) = containsMelds h [c]
    hasSon Nothing  = False

    threeMCheck     = containsMelds h (build3ConsT fstTriplet)
                   && (hasSon $ sonSequence)
    fourConsTCheck  = containsMelds h (build4ConsT fstTriplet)
    threeConsTCheck = containsMelds h (build3ConsT fstTriplet)
                   || containsMelds h (build3ConsT sndTriplet)

matchConsecutiveSets :: ScoreFunc
matchConsecutiveSets = distribute
  [ matchConsecutiveSequences, matchConsecutiveTriplets ]



{- 6.0 Suit Patterns -}

-- 6.1 Mixed and pure
matchOneSuit :: ScoreFunc
matchOneSuit (h, hs)
  | isSameTileType tiles = pure pureOneSuit
  | mixedCond            = pure mixedOneSuit
  | otherwise            = []
  where
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
-- | Moving this to matchSpecial scoring wise
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
matchSuits = distribute [ matchOneSuit ]



{- 7.0 Terminal Tiles -}

-- 7.1 Sequence and triplets
matchTerminals1 :: ScoreFunc
matchTerminals1 (h, hs)
  | or $ (containsMelds h) <$> pat = pure twoTailedTerminals
  | otherwise                      = accSequences ++ accTriplets
  where
    pat           = [ [c111, c123, c789, c999]
                    , [b111, b123, b789, b999]
                    , [k111, k123, k789, k999]
                    ]
    patSequences  = [ [c123, c789], [b123, b789], [k123, k789] ]
    patTriplets   = [ [c111, c999], [b111, b999], [k111, k999] ]
    sequenceCount = count (== True) . fmap (containsMelds h) $ patSequences
    tripletCount  = count (== True) . fmap (containsMelds h) $ patTriplets

    patSeqRepeat  = [ 1, 2, 3, 7, 8, 9 ] >>= (\x -> [x, x])
    nonEyes       = filter (not . isPair) $ getMelds h
    nonEyesTiles  = sort . fmap tileValue $ nonEyes >>= meldTiles
    isRepeated    = numOfSequences hs == 4
                 && all isTerminal nonEyes
                 && patSeqRepeat == nonEyesTiles
    repeatCount   = if isRepeated then 1 else 0

    accSequences  = take (sequenceCount + repeatCount) $ repeat twoTailedTerminalSequences
    accTriplets   = take tripletCount  $ repeat twoTailedTerminalTriplets

-- 7.2 Mixed and pure
matchTerminals2 :: ScoreFunc
matchTerminals2 (h, hs)
  | all isTerminal tiles                         = pure pureGreaterTerminals
  | all isTerminal melds && isSameTileType tiles = pure pureSuitTerminals
  | all isEdge     tiles                         = pure mixedGreaterTerminals
  | all isTerminal melds && hasSequences         = pure pureLesserTerminals
  | all isEdge     melds && hasSequences         = pure mixedLesserTerminals
  | otherwise                                    = []
  where
    melds        = getMelds h
    tiles        = getHandTiles h
    hasSequences = numOfSequences hs > 0

matchTerminals :: ScoreFunc
matchTerminals = distribute
  [ matchTerminals1, matchTerminals2 ]



{- 8.0 Honor Tiles -}

-- 8.1 Winds
matchWinds :: ScoreFunc
matchWinds (h, _)
  | numTriplets == 4                  = acc ++ pure bigFourWinds
  | numTriplets == 3 && numPairs == 1 = acc ++ pure littleFourWinds
  | numTriplets == 3 && numPairs == 0 = acc ++ pure bigThreeWinds
  | numTriplets == 2 && numPairs == 1 = acc ++ pure littleThreeWinds
  | otherwise                         = acc
  where
    windMelds   = filter isWind $ getMelds h
    numTriplets = count isTriplet windMelds
    numPairs    = count isPair windMelds
    acc         = take numTriplets $ repeat windTriplet

-- 8.2 Dragons
matchDragons :: ScoreFunc
matchDragons (h, _)
  | numTriplets == 3                  = acc ++ pure bigThreeDragons
  | numTriplets == 2 && numPairs == 1 = acc ++ pure littleThreeDragons
  | otherwise                         = acc
  where
    dragonMelds = filter isDragon $ getMelds h
    numTriplets = count isTriplet dragonMelds
    numPairs    = count isPair dragonMelds
    acc         = take numTriplets $ repeat dragonTriplet

-- 8.3 Pure honors
matchPureHonors :: ScoreFunc
matchPureHonors (h, _)
  | all isPair melds && match7Stars = pure allHonorPairs
  | all isHonor melds               = pure allHonors
  | otherwise                       = []
  where
    melds       = getMelds h
    match7Stars = sort (getHandTiles h) == pat
    pat         = honors >>= double
    double      = \x -> [x, x]

matchHonors :: ScoreFunc
matchHonors = distribute
  [ matchWinds, matchDragons, matchPureHonors ]



{- 9.0 Color Hands -}

matchColors :: ScoreFunc
matchColors (h, _)
  | all isGreen . getHandTiles $ h = pure allGreen
  | all isRed   . getHandTiles $ h = pure allRed
  | otherwise                      = []



{- 10.0 Irregular Hands -}

-- 10.1 Thirteen orphans

matchIrregular :: ScoreFunc
matchIrregular (h, _)
  | isSpecial h && isThirteenOrphans = pure thirteenOrphans
  | otherwise                        = []
  where
    isThirteenOrphans = (== edges) . sort . nub . getHandTiles $ h

-- 10.2 Seven pairs

matchSevenPairs :: ScoreFunc
matchSevenPairs (h, hs)
  | isSevenPairs && isShiftedPairs = pure sevenShiftedPairs
  | isSevenPairs                   = pure sevenPairs
  | otherwise                      = []
  where
    isSevenPairs   = numOfPairs hs == 7

    patMatcher     = matchValuePattern $ getHandTiles h
    isShiftedPairs = or . fmap patMatcher $ [pat1, pat2, pat3]

    pat1           = [1..7] >>= double
    pat2           = [2..8] >>= double
    pat3           = [3..9] >>= double
    double         = \x -> [x, x]



{- 11.0 Incidental bonuses -}

matchIncidentals :: ScoreFunc
matchIncidentals (h, _) =
  case getHandInfo h of
    Nothing -> []
    Just hi ->
      case hi of
        OnFirstDraw         -> pure blessingOfHeaven
        OnFirstDiscard      -> pure blessingOfEarth
        OnSeabed            -> pure finalDraw
        OnRiverbed          -> pure finalDiscard
        OnQuartetSupplement -> pure winOnQuartet
        OnBonusSupplement   -> pure winOnBonusTile
        OnQuartetRobbing    -> pure robbingAQuartet



{- 12.0 Bonus Tiles -}

matchBonus :: ScoreFunc
matchBonus (h, _)
  | numBonuses == 8 = pure allBonusTiles
  | numFlowers == 4 = pure fourFlowers
  | numSeasons == 4 = pure fourSeasons
  | numBonuses >  0 = pure $ updateScore bonusFlowerSeason numBonuses
  | otherwise       = []
  where
    numFlowers = count isFlower . getBonus $ h
    numSeasons = count isSeason . getBonus $ h
    numBonuses = numFlowers + numSeasons



{- Special hands -}

matchSpecial :: ScoreFunc
matchSpecial = distribute
  [ matchNineGates, matchIrregular ]

