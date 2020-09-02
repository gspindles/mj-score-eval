module Main where

import Game.Mahjong.Test.Class as CT
import Game.Mahjong.Test.Tile as TT
import Game.Mahjong.Test.Static.Tiles as TST
import Game.Mahjong.Test.Wall as WT
import Game.Mahjong.Test.Meld as MT
import Game.Mahjong.Test.Hand as HT
import Game.Mahjong.Test.HandStat as HST
import Game.Mahjong.Test.Score as ST

import Test.Tasty

main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "all tests" [
    CT.tests
  , TT.tests
  , TST.tests
  , WT.tests
  , MT.tests
  , HT.tests
  , HST.tests
  , ST.tests
  ]
