module Game.Mahjong.Test.Static.Melds ( tests ) where

import Game.Mahjong.Static.Melds

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Game.Mahjong.Static.Melds Tests" [countTests]


-- | Count tests

countTests :: TestTree
countTests = testGroup "Collections count tests" [
    testCase "There are 7 coin sequences" $
      length coinSequences @?= 7
  , testCase "There are 7 bamboo sequences" $
      length bambooSequences @?= 7
  , testCase "There are 7 character sequences" $
      length characterSequences @?= 7
  , testCase "There are 6 terminal sequences" $
      length terminalSequences @?= 6

  , testCase "There are 9 coin triplets" $
      length coinTriplets @?= 9
  , testCase "There are 9 bamboo triplets" $
      length bambooTriplets @?= 9
  , testCase "There are 9 character triplets" $
      length characterTriplets @?= 9
  , testCase "There are 6 terminal triplets" $
      length terminalTriplets @?= 6
  , testCase "There are 4 wind triplets" $
      length windTriplets @?= 4
  , testCase "There are 3 dragon triplets" $
      length dragonTriplets @?= 3

  , testCase "There are 9 coin quartets" $
      length coinQuartets @?= 9
  , testCase "There are 9 bamboo quartets" $
      length bambooQuartets @?= 9
  , testCase "There are 9 character quartets" $
      length characterQuartets @?= 9
  , testCase "There are 6 terminal quartets" $
      length terminalQuartets @?= 6
  , testCase "There are 4 wind quartets" $
      length windQuartets @?= 4
  , testCase "There are 3 dragon quartets" $
      length dragonQuartets @?= 3

  , testCase "There are 9 coin pairs" $
      length coinPairs @?= 9
  , testCase "There are 9 bamboo pairs" $
      length bambooPairs @?= 9
  , testCase "There are 9 character pairs" $
      length characterPairs @?= 9
  , testCase "There are 6 terminal pairs" $
      length terminalPairs @?= 6
  , testCase "There are 4 wind pairs" $
      length windPairs @?= 4
  , testCase "There are 3 dragon pairs" $
      length dragonPairs @?= 3

  , testCase "There are 21 sequence melds" $
      length sequenceMelds @?= 21
  , testCase "There are 34 triplet melds" $
      length tripletMelds @?= 34 
  , testCase "There are 34 quartet melds" $
      length quartetMelds @?= 34
  , testCase "There are 34 pair melds" $
      length pairMelds @?= 34 

  , testCase "There are 34 coin melds" $
      length coinMelds @?= 34 
  , testCase "There are 34 bamboo melds" $
      length bambooMelds @?= 34
  , testCase "There are 34 character melds" $
      length characterMelds @?= 34
  , testCase "There are 24 terminal melds" $
      length terminalMelds @?= 24
  , testCase "There are 12 wind melds" $
      length windMelds @?= 12
  , testCase "There are 9 dragon melds" $
      length dragonMelds @?= 9
  , testCase "There are 102 suit melds" $
      length suitMelds @?= 102
  , testCase "There are 21 honor melds" $
      length honorMelds @?= 21
  ]

