module Game.Mahjong.Test.Score ( tests ) where

import Game.Mahjong.Examples
import Game.Mahjong.Hand
import Game.Mahjong.Score
import Game.Mahjong.Pattern

import Control.Arrow ((&&&))
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit

containsPattern :: Maybe Hand -> Pattern -> Assertion
containsPattern h p = (@?= True) . elem p . snd . scoreHand $ h

containPattern :: [Maybe Hand] -> Pattern -> Assertion
containPattern hs p = (@?= True) . all (elem p . snd . scoreHand) $ hs

tests :: TestTree
tests = testGroup "Game.Mahjong.Score Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [scoreTests, functionTests]

scoreTests :: TestTree
scoreTests = testGroup "Meld creation tests" [
    trivialTests
  , pungsAndKongsTests
  , identicalSetsTests
  , similarSetsTests
  , consecutiveSetsTests
  , suitPatternsTests
  , terminalTests
  , honorTests
  , sevenPairsTests
  , colorTests
  , irregularTests
  , incidentalTests
  , bonusTess
  ]

-- 1.0 Trivial Patterns
trivialTests :: TestTree
trivialTests = testGroup "trival patterns test" [
    testCase "Chicken" $
      containsPattern chickenEx chicken
  , testCase "All Chows" $
      containsPattern allChowsEx allChows
  , testCase "Concealed" $
      containsPattern concealedEx concealed
  , testCase "Self Drawn" $
      containsPattern selfDrawnEx selfDrawn
  , testCase "All Simples" $
      containPattern [allSimplesEx1, allSimplesEx2] allSimples
  , testCase "All Types" $
      containPattern [allTypesEx1, allTypesEx2] allTypes
  , testCase "Illegal Call" $
      containsPattern illegalCallEx illegalCall
  ]

-- 2.0 Pungs and Kongs
pungsAndKongsTests :: TestTree
pungsAndKongsTests = testGroup "Pungs and Kongs Patterns test" [
    testCase "All Pungs" $
      containsPattern allPungsEx allPungs
  , testCase "Two Concealed Pungs" $
      containsPattern twoConcealedPungsEx twoConcealedPungs
  , testCase "Three Concealed Pungs" $
      containsPattern threeConcealedPungsEx threeConcealedPungs
  , testCase "Four Concealed Pungs" $
      containPattern [fourConcealedPungsEx1, fourConcealedPungsEx2] fourConcealedPungs
  , testCase "One Kong" $
      containsPattern oneKongEx oneKong
  , testCase "Two Kongs" $
      containsPattern twoKongsEx twoKongs
  , testCase "Three Kongs" $
      containsPattern threeKongsEx threeKongs
  , testCase "Four Kongs" $
      containsPattern fourKongsEx fourKongs
  ]

-- 3.0 Identical Sets
identicalSetsTests :: TestTree
identicalSetsTests = testGroup "Identical Sets test" [
    testCase "Two Identical Chows" $
      containsPattern twoIdenticalChowsEx twoIdenticalChows
  , testCase "Two Identical Chows Twice" $
      containsPattern twoIdenticalChowsTwiceEx twoIdenticalChowsTwice
  , testCase "Three Identical Chows" $
      containsPattern threeIdenticalChowsEx threeIdenticalChows
  , testCase "Four Identical Chows" $
      containsPattern fourIdenticalChowsEx fourIdenticalChows
  ]

-- 4.0 Similar Sets
similarSetsTests :: TestTree
similarSetsTests = testGroup "Similar Sets test" [
    testCase "Three Similar Chows" $
      containsPattern threeSimilarChowsEx threeSimilarChows
  , testCase "Little Three Similar Pungs" $
      containsPattern littleThreeSimilarPungsEx littleThreeSimilarPungs
  , testCase "Three Similar Pungs" $
      containsPattern threeSimilarPungsEx threeSimilarPungs
  ]

-- 5.0 Consecutive Sets
consecutiveSetsTests :: TestTree
consecutiveSetsTests = testGroup "Consecutive Sets test" [
    testCase "Three Consecutive Chows" $
      containPattern [threeConsecutiveChowsEx1, threeConsecutiveChowsEx2] threeConsecutiveChows
  , testCase "Nine Tile Straight" $
      containsPattern nineTileStraightEx nineTileStraight
  , testCase "Three Consecutive Chows Twice" $
      containPattern [threeConsecutiveChowsTwiceEx1, threeConsecutiveChowsTwiceEx2] threeConsecutiveChowsTwice
  , testCase "Four Consecutive Chows" $
      containPattern [fourConsecutiveChowsEx1, fourConsecutiveChowsEx2] fourConsecutiveChows
  , testCase "Three Consecutive Pungs" $
      containsPattern threeConsecutivePungsEx threeConsecutivePungs
  , testCase "Four Consecutive Pungs" $
      containPattern [fourConsecutivePungsEx1, fourConsecutivePungsEx2, fourConsecutivePungsEx3] fourConsecutivePungs
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
    testCase "Two Tailed Terminal Chows" $
      containPattern [twoTailedTerminalChowsEx1, twoTailedTerminalChowsEx2] twoTailedTerminalChows
  , testCase "Two Tailed Terminal Pungs" $
      containPattern [twoTailedTerminalPungsEx1, twoTailedTerminalPungsEx2] twoTailedTerminalPungs
  , testCase "Two Tailed Terminals" $
      containPattern [twoTailedTerminalsEx1, twoTailedTerminalsEx2, twoTailedTerminalsEx3, twoTailedTerminalsEx4] twoTailedTerminals
  , testCase "Little Boundless Mountain" $
      containPattern [littleBoundlessMountainEx1, littleBoundlessMountainEx2] littleBoundlessMountain
  , testCase "Big Boundless Mountain" $
      containPattern [bigBoundlessMountainEx1, bigBoundlessMountainEx2] bigBoundlessMountain
  , testCase "Mixed Lesser Terminals" $
      containsPattern mixedLesserTerminalsEx mixedLesserTerminals
  , testCase "Pure Lesser Terminals" $
      containsPattern pureLesserTerminalsEx pureLesserTerminals
  , testCase "Mixed Greater Terminals" $
      containPattern [mixedGreaterTerminalsEx1, mixedGreaterTerminalsEx2] mixedGreaterTerminals
  , testCase "Pure Greater Terminals" $
      containPattern [pureGreaterTerminalsEx1, pureGreaterTerminalsEx2] pureGreaterTerminals
  ]

-- 8.0 Honor Tiles
honorTests :: TestTree
honorTests = testGroup "Honor Hands test" [
    testCase "Wind Pung" $
      containsPattern windPungEx windPung
  , testCase "Little Three Winds" $
      containsPattern littleThreeWindsEx littleThreeWinds
  , testCase "Big Three Winds" $
      containsPattern bigThreeWindsEx bigThreeWinds
  , testCase "Little Four Winds" $
      containsPattern littleFourWindsEx littleFourWinds
  , testCase "Big Four Winds" $
      containsPattern bigFourWindsEx bigFourWinds
  , testCase "Dragon Pung" $
      containsPattern dragonPungEx dragonPung
  , testCase "Little Three Dragons" $
      containsPattern littleThreeDragonsEx littleThreeDragons
  , testCase "Big Three Dragon" $
      containsPattern bigThreeDragonsEx bigThreeDragons
  , testCase "All Honors" $
      containPattern [allHonorsEx1, allHonorsEx2] allHonors
  , testCase "All Honor Pairs" $
      containsPattern allHonorPairsEx allHonorPairs
  ]

-- 9.0 Seven Pairs
sevenPairsTests :: TestTree
sevenPairsTests = testGroup "Sever Pairs Hands test" [
    testCase "Seven Pairs" $
      containPattern [sevenPairsEx1, sevenPairsEx2] sevenPairs
  , testCase "Seven Shifted Pairs" $
      containPattern [sevenShiftedPairsEx1, sevenShiftedPairsEx2] sevenShiftedPairs
  , testCase "Grand Chariot" $
      containsPattern grandChariotEx grandChariot
  , testCase "Bamboo Forest" $
      containsPattern bambooForestEx bambooForest
  , testCase "Number Neighborhood" $
      containsPattern numberNeighborhoodEx numberNeighborhood
  ]

-- 10.0 Color Hands
colorTests :: TestTree
colorTests = testGroup "Color Hands test" [
    testCase "All Green" $
      containPattern [allGreenEx1, allGreenEx2] allGreen
  , testCase "All Red" $
      containsPattern allRedEx allRed
  ]

-- 11.0 Irregular Hands
irregularTests :: TestTree
irregularTests = testGroup "Irregular Hands test" [
    testCase "Thirtheen Orphans" $
      containPattern [thirteenOrphansImpureEx, thirteenOrphansPureEx] thirteenOrphans
  ]

-- 12.0 Incidental bonuses
incidentalTests :: TestTree
incidentalTests = testGroup "Incidental Bonuses test" [
    testCase "Final Draw" $
      containsPattern finalDrawEx finalDraw
  , testCase "Final Discard" $
      containsPattern finalDiscardEx finalDiscard
  , testCase "Win on Kong" $
      containsPattern winOnKongEx winOnKong
  , testCase "Win on Bonus Tile" $
      containsPattern winOnBonusTileEx winOnBonusTile
  , testCase "Robbing a Kong" $
      containsPattern robbingAKongEx robbingAKong
  , testCase "Blessing of Heaven" $
      containsPattern blessingOfHeavenEx blessingOfHeaven
  , testCase "Blessing of Earth" $
      containsPattern blessingOfEarthEx blessingOfEarth
  ]

-- 13.0 Bonus Tiles
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
    ps1  = [chicken, illegalCall]
    ps2  = [littleThreeWinds, mixedOneSuit, allPungs]
    ps3a = [allHonors, bigFourWinds, allPungs]
    ps3b = [allHonors, bigFourWinds]
