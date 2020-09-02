{-# LANGUAGE TypeApplications #-}

module Game.Mahjong.Test.HandStat ( tests ) where

import Game.Mahjong.Meld
import Game.Mahjong.Hand
import Game.Mahjong.HandStat
import Game.Mahjong.Static.Examples (allTypesEx1)

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Data.Maybe (fromJust)

instance Arbitrary HandStat where
  arbitrary = do
    Positive numOfCoins      <- arbitrary
    Positive numOfBamboos    <- arbitrary
    Positive numOfCharacters <- arbitrary
    Positive numOfWinds      <- arbitrary
    Positive numOfDragons    <- arbitrary
    Positive numOfSimples    <- arbitrary
    Positive numOfTerminals  <- arbitrary
    Positive numOfSequences  <- arbitrary
    Positive numOfTriplets   <- arbitrary
    Positive numOfQuartets   <- arbitrary
    Positive numOfPairs      <- arbitrary
    pure $ HandStat numOfCoins numOfBamboos numOfCharacters
                    numOfWinds numOfDragons
                    numOfSimples numOfTerminals
                    numOfSequences numOfTriplets numOfQuartets numOfPairs

tests :: TestTree
tests = testGroup "Game.Mahjong.HandStat Tests" [
    propertyTests
  , countTests
  ]


-- | Property tests
propertyTests :: TestTree
propertyTests = testGroup "Properties tests" [
    qcPropertyTests
  , qcCountTests
  ]

qcPropertyTests :: TestTree
qcPropertyTests = testGroup "QuickCheck property tests" [
    QC.testProperty "identity on right" $
      \hs -> ((hs :: HandStat) <> mempty)
          == hs
  , QC.testProperty "identity on left" $
      \hs -> (hs :: HandStat)
          == mempty <> hs
  , QC.testProperty "communitivity" $
      \hs1 hs2 -> ((hs1 :: HandStat) <> (hs2 :: HandStat))
                == (hs2 <> hs1)
  , QC.testProperty "associativity" $
      \hs1 hs2 hs3 -> (((hs1 :: HandStat) <> (hs2 :: HandStat)) <> (hs3 :: HandStat))
                   == (hs1 <> (hs2 <> hs3))
  ]

qcCountTests :: TestTree
qcCountTests = testGroup "QuickCheck count tests" [
    QC.testProperty "count numberOfSuits" $
      \hs -> (numOfSuits (hs :: HandStat))
          == (sum $ fmap ($ hs) [numOfCoins, numOfBamboos, numOfCharacters])
  , QC.testProperty "count numberOfHonors" $
      \hs -> (numOfHonors (hs :: HandStat))
          == (sum $ fmap ($ hs) [numOfWinds, numOfDragons])
  , QC.testProperty "count numberOfEdges" $
      \hs -> (numOfEdges (hs :: HandStat))
          == (sum $ fmap ($ hs) [numOfTerminals, numOfWinds, numOfDragons])
  , QC.testProperty "count numberOfMelds" $
      \hs -> (numOfMelds (hs :: HandStat))
          == (sum $ fmap ($ hs) [numOfSequences, numOfTriplets, numOfPairs])
  ]


-- Count tests

countTests :: TestTree
countTests = testGroup "Count tests" [
    testCase "count numOfCoins" $
      numOfCoins stat @?= 1
  , testCase "count numOfBamboos" $
      numOfBamboos stat @?= 1
  , testCase "count numOfCharacters" $
      numOfCharacters stat @?= 1
  , testCase "count numOfWinds" $
      numOfWinds stat @?= 1
  , testCase "count numOfDragons" $
      numOfDragons stat @?= 1
  , testCase "count numOfSimples" $
      numOfSimples stat @?= 2
  , testCase "count numOfTerminals" $
      numOfTerminals stat @?= 1
  , testCase "count numOfSequences" $
      numOfSequences stat @?= 2
  , testCase "count numOfTriplets" $
      numOfTriplets stat @?= 2
  , testCase "count numOfQuartets" $
      numOfQuartets stat @?= 1
  , testCase "count numOfPairs" $
      numOfPairs stat @?= 1
  , testCase "count numOfSuits" $
      numOfSuits stat @?= 3
  , testCase "count numOfHonors" $
      numOfHonors stat @?= 2
  , testCase "count numOfEdges" $
      numOfEdges stat @?= 3
  , testCase "count numOfMelds" $
      numOfMelds stat @?= 5
  ]
  where
    stat = handStat . fromJust $ allTypesEx1

