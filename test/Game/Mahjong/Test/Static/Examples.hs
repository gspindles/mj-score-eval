module Game.Mahjong.Test.Static.Examples ( tests ) where

import Game.Mahjong.Meld
import Game.Mahjong.Static.Tiles
import Game.Mahjong.Static.Melds
import Game.Mahjong.Static.Examples

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Game.Mahjong.Static.Examples Tests" [constructionTests]


-- | Construction tests

constructionTests :: TestTree
constructionTests = testGroup "Construction tests" [
    mkSequenceTests
  , mkTripletTests
  , mkQuartetTests
  , mkPairTests
  ]

mkSequenceTests :: TestTree
mkSequenceTests = testGroup "Sequence creation tests" [
    testCase "mkSequence' fails with Promote status" $
      mkSequence' Promoted c1 @?= Nothing
  , testCase "mkSequence' fails with bonus tile" $
      mkSequence' Revealed f1 @?= Nothing
  , testCase "mkSequence' fails with honor tile" $
      mkSequence' Revealed ww @?= Nothing
  , testCase "mkSequence' passes" $
      mkSequence' Revealed c1 @?= Just c123
  ]

mkTripletTests :: TestTree
mkTripletTests = testGroup "Triplet creation tests" [
    testCase "mkTriplet' fails with Promote status" $
      mkTriplet' Promoted c1 @?= Nothing
  , testCase "mkTriplet' fails with bonus tile" $
      mkTriplet' Revealed s1 @?= Nothing
  , testCase "mkTriplet' succeeds" $
      mkTriplet' Revealed c1 @?= Just c111
  , testCase "mkTriplet' succeeds" $
      mkTriplet' Revealed wn @?= Just wnnn
  ]

mkQuartetTests :: TestTree
mkQuartetTests = testGroup "Quartet creation tests" [
    testCase "mkQuartet' passes with Promote status" $
      mkQuartet' Promoted c1 @?= promoteTriplet c111 c1
  , testCase "mkQuartet' fails with bonus tile" $
      mkQuartet' Revealed f1 @?= Nothing
  , testCase "mkQuartet' succeeds" $
      mkQuartet' Revealed c1 @?= Just c1111
  , testCase "mkQuartet' succeeds" $
      mkQuartet' Revealed wn @?= Just wnnnn
  ]

mkPairTests :: TestTree
mkPairTests = testGroup "Pair creation tests" [
    testCase "mkPair' fails with Promote status" $
      mkPair' Promoted c1 @?= Nothing
  , testCase "mkPair' fails with bonus tile" $
      mkPair' Revealed s1 @?= Nothing
  , testCase "mkPair' succeeds" $
      mkPair' Revealed c1 @?= Just c11
  , testCase "mkPair' succeeds" $
      mkPair' Revealed dr @?= Just drr
  ]

