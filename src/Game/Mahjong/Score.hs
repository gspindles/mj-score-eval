{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Functions for scoring mahjong patterns
--   and evaluate score
module Game.Mahjong.Score (
    -- ** score functions
    scoreHand, calculateScore, matchForPatterns
) where

import Game.Mahjong.Hand
import Game.Mahjong.Meld
import Game.Mahjong.Class
import Game.Mahjong.Tile
import Game.Mahjong.Pattern

import Control.Arrow ((&&&))
import Control.Applicative (liftA2)
import Data.Foldable (foldr1, maximumBy)
import Data.List (group, groupBy, inits, intersect, nub, sort, sortBy, tails)
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
  then pure chicken
  else sortPatterns $ patterns ++ bonusPats
  where
    stat      = (hand, handStat hand)
    patterns  | isSpecial hand = matchSpecial stat
              | otherwise      = (<->>) matchers stat
    matchers  = [ matchTrivials         -- 1.  Trivival Patterns
                , matchPungsAndKongs    -- 2.  Pungs & Kongs
                , matchIdenticalSets    -- 3.  Identical Sets
                , matchSimilarSets      -- 4.  Similar Sets
                , matchConsecutiveSets  -- 5.  Consecutive Sets
                , matchSuits            -- 6.  Suit Patterns
                , matchTerminals        -- 7.  Terminal Tiles
                , matchHonors           -- 8.  Honor Tiles
                , matchSevenPairs       -- 9.  Seven Pairs
                , matchColors           -- 10. Colors Hands
                , matchIncidentals      -- 12. Incidental Bonuses
                ]
    bonusPats = matchBonus stat         -- 13. Bonus Tiles


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

histogram :: Ord a => [a] -> [(a, Int)]
histogram = fmap bar . group . sort
  where
    bar x = (head x, length x)

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



{- 2.0 Pungs and Kongs -}

-- 2.1 Pung
matchPungs :: ScoreFunc
matchPungs (_, hs)
  | numOfPungs hs >= 4 = pure allPungs
  | otherwise          = []

-- 2.2 Concealed pungs
matchConcealedPungs :: ScoreFunc
matchConcealedPungs (h, _)
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

    counts    = length <$> twoOrMore
    twoCount  = count (== 2) counts



{- 4.0 Similar Sets -}

-- 4.1 Similar chows
-- 4.2 Similar pungs
matchSimilarSets :: ScoreFunc
matchSimilarSets (h, _)
  | similarCheck intersectionChow  = pure threeSimilarChows
  | similarCheck intersectionPung  = pure threeSimilarPungs
  | length eye == 1 && hasLTSP eye = pure littleThreeSimilarPungs
  | otherwise                      = []
  where
    melds            = getMelds h
    similarCheck     = getAny . foldMap (Any . not . null)

    checkChow        = \m -> isChow m && isSuit m
    projectedChow    = filterAndGroupByTileType checkChow melds
    intersectionChow = if length projectedChow == 3
                       then commonElems projectedChow
                       else []

    checkPung        = \m -> isPung m && isSuit m
    projectedPung    = filterAndGroupByTileType checkPung melds
    intersectionPung = if length projectedPung == 3
                       then commonElems projectedPung
                       else []

    eye              = filter (\m -> isEyes m && isSuit m) melds
    hasLTSP          = containsMelds h . getLTSP . head . meldTiles . head
    getLTSP et
      | et == c1  = [ b111, k111 ]
      | et == c2  = [ b222, k222 ]
      | et == c3  = [ b333, k333 ]
      | et == c4  = [ b444, k444 ]
      | et == c5  = [ b555, k555 ]
      | et == c6  = [ b666, k666 ]
      | et == c7  = [ b777, k777 ]
      | et == c8  = [ b888, k888 ]
      | et == c9  = [ b999, k999 ]

      | et == b1  = [ c111, k111 ]
      | et == b2  = [ c222, k222 ]
      | et == b3  = [ c333, k333 ]
      | et == b4  = [ c444, k444 ]
      | et == b5  = [ c555, k555 ]
      | et == b6  = [ c666, k666 ]
      | et == b7  = [ c777, k777 ]
      | et == b8  = [ c888, k888 ]
      | et == b9  = [ c999, k999 ]

      | et == k1  = [ c111, b111 ]
      | et == k2  = [ c222, b222 ]
      | et == k3  = [ c333, b333 ]
      | et == k4  = [ c444, b444 ]
      | et == k5  = [ c555, b555 ]
      | et == k6  = [ c666, b666 ]
      | et == k7  = [ c777, b777 ]
      | et == k8  = [ c888, b888 ]
      | et == k9  = [ c999, b999 ]

      | otherwise = []  -- shouldn't get here



{- 5.0 Consecutive Sets -}

-- 5.1 Consecutive chows
matchConsecutiveChows :: ScoreFunc
matchConsecutiveChows (h, _)
  | length sorted == 4 && fourConsCCheck   = pure fourConsecutiveChows
  | length sorted == 4 && threeConsCTCheck = pure threeConsecutiveChowsTwice
  | length sorted >= 3 && nineTSCheck      = pure nineTileStraight
  | length sorted >= 3 && threeConsCCheck  = pure threeConsecutiveChows
  | otherwise                              = []
  where
    melds            = getMelds h
    chows            = filter (\m -> isChow m && isSuit m) melds
    sorted           = sortBy (comparing meldTiles) chows
    fstChow          = head sorted
    sndChow          = head $ tail sorted

    build4ConsC1     = take 4 . iterate next
    build4ConsC2     = take 4 . iterate (next . next)
    build3ConsCT1 m  = [ m
                       , next m
                       , next (next m)
                       , next (next (next (next m)))
                       ]
    build3ConsCT2 m  = [ m
                       , next (next m)
                       , next (next (next m))
                       , next (next (next (next m)))
                       ]
    build3ConsC1     = take 3 . iterate next
    build3ConsC2     = take 3 . iterate (next . next)
    build3ConsC3     = take 3 . iterate (next . next . next)
    chow123s m       = any (meldTileMatch True m) [ c123, b123, k123 ]

    fourConsCCheck   = ( chow123s fstChow
                    && containsMelds h (build4ConsC2 fstChow) )
                    || containsMelds h (build4ConsC1 fstChow)
    threeConsCTCheck = containsMelds h (build3ConsCT1 fstChow)
                    || containsMelds h (build3ConsCT2 fstChow)
    nineTSCheck      = ( chow123s fstChow
                    && containsMelds h (build3ConsC3 fstChow) )
                    || ( chow123s sndChow
                    && containsMelds h (build3ConsC2 sndChow) )
    threeConsCCheck  = containsMelds h (build3ConsC1 fstChow)
                    || containsMelds h (build3ConsC2 fstChow)
                    || containsMelds h (build3ConsC1 sndChow)
                    || containsMelds h (build3ConsC2 sndChow)

-- 5.2 Consecutive pungs
matchConsecutivePungs :: ScoreFunc
matchConsecutivePungs (h, _)
  | length sorted == 3 && threeMCheck     = pure threeMothers
  | length sorted == 4 && fourConsPCheck  = pure fourConsecutivePungs
  | length sorted >= 3 && threeConsPCheck = pure threeConsecutivePungs
  | otherwise                             = []
  where
    melds           = getMelds h
    pungs           = filter (\m -> isPung m && isSuit m) melds
    sorted          = sortBy (comparing meldTiles) pungs
    fstPung         = head sorted
    sndPung         = head $ tail sorted
    sonTiles        = fmap (head . meldTiles) sorted

    build3ConsP     = take 3 . iterate next
    build4ConsP     = take 4 . iterate next
    sonChow         = mkChow Revealed sonTiles
    hasSon (Just c) = containsMelds h [c]
    hasSon Nothing  = False

    threeMCheck     = containsMelds h (build3ConsP fstPung)
                   && (hasSon $ sonChow)
    fourConsPCheck  = containsMelds h (build4ConsP fstPung)
    threeConsPCheck = containsMelds h (build3ConsP fstPung)
                   || containsMelds h (build3ConsP sndPung)

matchConsecutiveSets :: ScoreFunc
matchConsecutiveSets = (<->>) [ matchConsecutiveChows
                              , matchConsecutivePungs
                              ]



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
matchSuits = (<->>) [ matchOneSuit ]



{- 7.0 Terminal Tiles -}

-- 7.1 Chow and pungs
matchTerminals1 :: ScoreFunc
matchTerminals1 (h, _)
  | isSameTileType tiles && matchBBM = acc ++ pure bigBoundlessMountain
  | isSameTileType tiles && matchLBM = acc ++ pure littleBoundlessMountain
  | or $ (containsMelds h) <$> pat2  = acc ++ pure twoTailedTerminals
  | otherwise                        = acc
  where
    melds    = getMelds h
    tiles    = getHandTiles h
    patBM1   = [1, 1, 1, 1, 2, 2, 3, 3, 7, 8, 9, 9, 9, 9]
    patBM2   = [1, 1, 1, 1, 2, 3, 7, 7, 8, 8, 9, 9, 9, 9]
    matchBBM = matchValuePattern tiles patBM1
            || matchValuePattern tiles patBM2
    matchLBM = all isTerminal melds

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
    acc       = accChows ++ accPungs

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
  | isSpecial h && isThirteenOrphans = pure thirteenOrphans
  | otherwise                        = []
  where
    isThirteenOrphans = (== edges) . sort . nub . getHandTiles $ h



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
  | numBonuses >  0 = pure $ updateScore bonusFlowerSeason numBonuses
  | otherwise       = []
  where
    numFlowers = count isFlower . getBonus $ h
    numSeasons = count isSeason . getBonus $ h
    numBonuses = numFlowers + numSeasons



{- Special hands -}

matchSpecial :: ScoreFunc
matchSpecial = (<->>) [ matchNineGates
                      , matchIrregular
                      ]
