module Main where

import Game.Mahjong.Test.Class as CT
import Game.Mahjong.Test.Tile as TT
import Game.Mahjong.Test.Meld as MT
import Game.Mahjong.Test.Score as ST

import Test.Tasty

main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "all tests" [
    CT.tests
  , TT.tests
  , MT.tests
  , ST.tests
  ]
