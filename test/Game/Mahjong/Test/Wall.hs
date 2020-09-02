module Game.Mahjong.Test.Wall ( tests ) where

import Game.Mahjong.Wall

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit

import Data.List (nub, sort)

instance Arbitrary WithBonus where
    arbitrary = elements [Exclude, Include]

tests :: TestTree
tests = testGroup "Game.Mahjong.Wall Tests" [countTests]


-- | Count tests

countTests :: TestTree
countTests = testGroup "Wall count tests" [
    testCase "There are 136 tiles in a set without bonus" $
      length (mjSet Exclude) @?= 136
  , testCase "There are 144 tiles in a set with bonus" $
      length (mjSet Include) @?= 144
  , testCase "There are 34 different tiles in a set without bonus" $
      (length . nub . sort $ mjSet Exclude) @?= 34
  , testCase "There are 42 different tiles in a set with bonus" $
      (length . nub . sort $ mjSet Include) @?= 42
  , testCase "There are 136 tiles in a wall without bonus" $
      length (getWall Exclude 1) @?= 136
  , testCase "There are 144 tiles in a wall with bonus" $
      length (getWall Include 1) @?= 144
  ]

