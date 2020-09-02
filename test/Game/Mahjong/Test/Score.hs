module Game.Mahjong.Test.Score ( tests ) where

import Game.Mahjong.Static.Examples
import Game.Mahjong.Hand
import Game.Mahjong.Score
import Game.Mahjong.Pattern

import Test.Tasty
import Test.Tasty.HUnit

import Control.Arrow ((&&&))

containsPattern :: Maybe Hand -> Pattern -> Assertion
containsPattern h p = (@?= True) . elem p . snd . scoreHand $ h

containPattern :: [Maybe Hand] -> Pattern -> Assertion
containPattern hs p = (@?= True) . all (elem p . snd . scoreHand) $ hs

tests :: TestTree
tests = testGroup "Game.Mahjong.Score Tests" [
    scoreTests
  , functionTests
  ]


-- | Score tests

scoreTests :: TestTree
scoreTests = testGroup "Meld creation tests" [
    trivialTests
  , tripletsAndQuartetsTests
  , identicalSetsTests
  , similarSetsTests
  , consecutiveSetsTests
  , suitPatternsTests
  , terminalTests
  , honorTests
  , colorTests
  , irregularTests
  , incidentalTests
  , bonusTess
  ]

-- 1.0 Trivial Patterns
trivialTests :: TestTree
trivialTests = testGroup "trival patterns test" [
    testCase "Chicken" $
      containsPattern chickenEx chickenHand
  , testCase "All Sequences" $
      containsPattern allSequencesEx allSequences
  , testCase "Concealed" $
      containsPattern concealedEx concealedHand
  , testCase "Self Drawn" $
      containsPattern selfDrawnEx selfDrawn
  , testCase "All Simples" $
      containPattern [allSimplesEx1, allSimplesEx2] allSimples
  , testCase "All Types" $
      containPattern [allTypesEx1, allTypesEx2] allTypes
  , testCase "Illegal Call" $
      containsPattern illegalCallEx illegalCall
  ]

-- 2.0 Triplets and Quartets
tripletsAndQuartetsTests :: TestTree
tripletsAndQuartetsTests = testGroup "Triplets and Quartets Patterns test" [
    testCase "All Triplets" $
      containsPattern allTripletsEx allTriplets
  , testCase "Two Concealed Triplets" $
      containsPattern twoConcealedTripletsEx twoConcealedTriplets
  , testCase "Three Concealed Triplets" $
      containsPattern threeConcealedTripletsEx threeConcealedTriplets
  , testCase "Four Concealed Triplets" $
      containPattern [fourConcealedTripletsEx1, fourConcealedTripletsEx2] fourConcealedTriplets
  , testCase "One Quartet" $
      containsPattern oneQuartetEx oneQuartet
  , testCase "Two Quartets" $
      containsPattern twoQuartetsEx twoQuartets
  , testCase "Three Quartets" $
      containsPattern threeQuartetsEx threeQuartets
  , testCase "Four Quartets" $
      containsPattern fourQuartetsEx fourQuartets
  ]

-- 3.0 Identical Sets
identicalSetsTests :: TestTree
identicalSetsTests = testGroup "Identical Sets test" [
    testCase "Two Identical Sequences" $
      containsPattern twoIdenticalSequencesEx twoIdenticalSequences
  , testCase "Two Identical Sequences Twice" $
      containsPattern twoIdenticalSequencesTwiceEx twoIdenticalSequencesTwice
  , testCase "Three Identical Sequences" $
      containsPattern threeIdenticalSequencesEx threeIdenticalSequences
  , testCase "Four Identical Sequences" $
      containsPattern fourIdenticalSequencesEx fourIdenticalSequences
  ]

-- 4.0 Similar Sets
similarSetsTests :: TestTree
similarSetsTests = testGroup "Similar Sets test" [
    testCase "Three Similar Sequences" $
      containsPattern threeSimilarSequencesEx threeSimilarSequences
  , testCase "Little Three Similar Triplets" $
      containsPattern littleThreeSimilarTripletsEx littleThreeSimilarTriplets
  , testCase "Three Similar Triplets" $
      containsPattern threeSimilarTripletsEx threeSimilarTriplets
  ]

-- 5.0 Consecutive Sets
consecutiveSetsTests :: TestTree
consecutiveSetsTests = testGroup "Consecutive Sets test" [
    testCase "Three Consecutive Sequences" $
      containPattern [threeConsecutiveSequencesEx1, threeConsecutiveSequencesEx2] threeConsecutiveSequences
  , testCase "Nine Tile Straight" $
      containsPattern nineTileStraightEx nineTileStraight
  , testCase "Three Consecutive Sequences Twice" $
      containPattern [threeConsecutiveSequencesTwiceEx1, threeConsecutiveSequencesTwiceEx2] threeConsecutiveSequencesTwice
  , testCase "Four Consecutive Sequences" $
      containPattern [fourConsecutiveSequencesEx1, fourConsecutiveSequencesEx2] fourConsecutiveSequences
  , testCase "Three Consecutive Triplets" $
      containsPattern threeConsecutiveTripletsEx threeConsecutiveTriplets
  , testCase "Four Consecutive Triplets" $
      containPattern [fourConsecutiveTripletsEx1, fourConsecutiveTripletsEx2, fourConsecutiveTripletsEx3] fourConsecutiveTriplets
  , testCase "Three Mothers" $
      containsPattern threeMothersEx threeMothers
  ]

-- 6.0 Suit Patterns
suitPatternsTests :: TestTree
suitPatternsTests = testGroup "Suit Patterns test" [
    testCase "Mixed One Suit" $
      containPattern [mixedOneSuitEx1, mixedOneSuitEx2] mixedOneSuit
  , testCase "Pure One Suit" $
      containPattern [pureOneSuitEx1, pureOneSuitEx2] pureOneSuit
  , testCase "Nine Gates" $
      containsPattern nineGatesEx nineGates
  ]

-- 7.0 Terminal Tiles
terminalTests :: TestTree
terminalTests = testGroup "Terminal Hands test" [
    testCase "Two Tailed Terminal Sequences" $
      containPattern [twoTailedTerminalSequencesEx1, twoTailedTerminalSequencesEx2, twoTailedTerminalSequencesEx3] twoTailedTerminalSequences
  , testCase "Two Tailed Terminal Triplets" $
      containPattern [twoTailedTerminalTripletsEx1, twoTailedTerminalTripletsEx2] twoTailedTerminalTriplets
  , testCase "Two Tailed Terminals" $
      containPattern [twoTailedTerminalsEx1, twoTailedTerminalsEx2, twoTailedTerminalsEx3, twoTailedTerminalsEx4] twoTailedTerminals
  , testCase "Mixed Lesser Terminals" $
      containsPattern mixedLesserTerminalsEx mixedLesserTerminals
  , testCase "Pure Lesser Terminals" $
      containsPattern pureLesserTerminalsEx pureLesserTerminals
  , testCase "Mixed Greater Terminals" $
      containPattern [mixedGreaterTerminalsEx1, mixedGreaterTerminalsEx2] mixedGreaterTerminals
  , testCase "Pure Suit Terminals" $
      containPattern [pureSuitTerminalsEx1, pureSuitTerminalsEx2, pureSuitTerminalsEx3, pureSuitTerminalsEx4] pureSuitTerminals
  , testCase "Pure Greater Terminals" $
      containPattern [pureGreaterTerminalsEx1, pureGreaterTerminalsEx2] pureGreaterTerminals
  ]

-- 8.0 Honor Tiles
honorTests :: TestTree
honorTests = testGroup "Honor Hands test" [
    testCase "Wind Triplet" $
      containsPattern windTripletEx windTriplet
  , testCase "Little Three Winds" $
      containsPattern littleThreeWindsEx littleThreeWinds
  , testCase "Big Three Winds" $
      containsPattern bigThreeWindsEx bigThreeWinds
  , testCase "Little Four Winds" $
      containsPattern littleFourWindsEx littleFourWinds
  , testCase "Big Four Winds" $
      containsPattern bigFourWindsEx bigFourWinds
  , testCase "Dragon Triplet" $
      containPattern [dragonTripletEx1, dragonTripletEx2] dragonTriplet
  , testCase "Little Three Dragons" $
      containsPattern littleThreeDragonsEx littleThreeDragons
  , testCase "Big Three Dragon" $
      containsPattern bigThreeDragonsEx bigThreeDragons
  , testCase "All Honors" $
      containPattern [allHonorsEx1, allHonorsEx2] allHonors
  , testCase "All Honor Pairs" $
      containsPattern allHonorPairsEx allHonorPairs
  ]

-- 9.0 Color Hands
colorTests :: TestTree
colorTests = testGroup "Color Hands test" [
    testCase "All Green" $
      containPattern [allGreenEx1, allGreenEx2] allGreen
  , testCase "All Red" $
      containsPattern allRedEx allRed
  ]

-- 10.0 Irregular Hands
irregularTests :: TestTree
irregularTests = testGroup "Irregular Hands test" [
    testCase "Thirtheen Orphans" $
      containPattern [thirteenOrphansImpureEx, thirteenOrphansPureEx] thirteenOrphans
  , testCase "Seven Pairs" $
      containPattern [sevenPairsEx1, sevenPairsEx2] sevenPairs
  , testCase "Seven Consecutive Pairs" $
      containPattern [sevenConsecutivePairsEx1, sevenConsecutivePairsEx2, grandChariotEx, bambooForestEx, numerousNeighborsEx] sevenConsecutivePairs
  ]

-- 11.0 Incidental bonuses
incidentalTests :: TestTree
incidentalTests = testGroup "Incidental Bonuses test" [
    testCase "Final Draw" $
      containsPattern finalDrawEx finalDraw
  , testCase "Final Discard" $
      containsPattern finalDiscardEx finalDiscard
  , testCase "Win on Quartet" $
      containsPattern winOnQuartetEx winOnQuartet
  , testCase "Win on Bonus Tile" $
      containsPattern winOnBonusTileEx winOnBonusTile
  , testCase "Robbing a Quartet" $
      containsPattern robbingAQuartetEx robbingAQuartet
  , testCase "Blessing of Heaven" $
      containsPattern blessingOfHeavenEx blessingOfHeaven
  , testCase "Blessing of Earth" $
      containsPattern blessingOfEarthEx blessingOfEarth
  ]

-- 12.0 Bonus Tiles
bonusTess :: TestTree
bonusTess = testGroup "Bonus Tiles test" [
    testCase "Bonus Tile" $
      containsPattern bonusTilesEx bonusFlowerSeason
  , testCase "Four Flowers" $
      containsPattern fourFlowersEx fourFlowers
  , testCase "Four Seasons" $
      containsPattern fourSeasonsEx fourSeasons
  , testCase "All Bonus Tiles" $
      containsPattern allBonusTilesEx allBonusTiles
  ]


-- | Function tests

functionTests :: TestTree
functionTests = testGroup "Function tests" [
    testCase "scoreHand test on Nothing" $
      scoreHand Nothing @?= ic
  , testCase "calculateScore test - patterns contains illegalCall" $
      calculateScore ps1 @?= ic
  , testCase "calculateScore test - no limit hands" $
      calculateScore ps2 @?= (sum $ score <$> ps2, ps2)
  , testCase "calculateScore test - has limit hands" $
      calculateScore ps3a @?= (maximum $ score <$> ps3a, ps3b)
  ]
  where
    ic   = score &&& pure $ illegalCall
    ps1  = [chickenHand, illegalCall]
    ps2  = [littleThreeWinds, mixedOneSuit, allTriplets]
    ps3a = [allHonors, bigFourWinds, allTriplets]
    ps3b = [allHonors, bigFourWinds]

