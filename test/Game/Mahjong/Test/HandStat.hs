{-# LANGUAGE TypeApplications #-}

module Game.Mahjong.Test.HandStat ( tests ) where

import Game.Mahjong.Meld
import Game.Mahjong.Hand
import Game.Mahjong.HandStat

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

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
tests = testGroup "Game.Mahjong.HandStat Tests" [propertyTests, countTests]

propertyTests :: TestTree
propertyTests = testGroup "Properties tests" [qcPropertyTests, countTests]

qcPropertyTests :: TestTree
qcPropertyTests = testGroup "QuickCheck property tests" [
    QC.testProperty "identity 1" $
      \hs -> ((hs :: HandStat) <> mempty)
          == hs
  , QC.testProperty "identity 2" $
      \hs -> (hs :: HandStat)
          == mempty <> hs
  , QC.testProperty "communitivity" $
      \hs1 hs2 -> ((hs1 :: HandStat) <> (hs2 :: HandStat))
                == (hs2 <> hs1)
  , QC.testProperty "associativity" $
      \hs1 hs2 hs3 -> (((hs1 :: HandStat) <> (hs2 :: HandStat)) <> (hs3 :: HandStat))
                   == (hs1 <> (hs2 <> hs3))
  ]

countTests :: TestTree
countTests = testGroup "Count tests" []

