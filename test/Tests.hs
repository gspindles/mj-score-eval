module Main where

import Game.Mahjong.Test.Class as CT
import Game.Mahjong.Test.Tile as TT
import Game.Mahjong.Test.Static.Tiles as STT
import Game.Mahjong.Test.Wall as WT
import Game.Mahjong.Test.Meld as MT
import Game.Mahjong.Test.Static.Melds as SMT
import Game.Mahjong.Test.Hand as HT
import Game.Mahjong.Test.HandStat as HST
import Game.Mahjong.Test.Static.Examples as SET
import Game.Mahjong.Test.Score as ST

import Test.Tasty

main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "all tests" [
    CT.tests
  , TT.tests
  , STT.tests
  , WT.tests
  , MT.tests
  , SMT.tests
  , HT.tests
  , HST.tests
  , SET.tests
  , ST.tests
  ]
