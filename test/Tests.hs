module Main where

import Game.Mahjong.Test.Class as CT
import Game.Mahjong.Test.Tile as TT

import Test.Tasty

main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "all tests" [
    CT.tests
  , TT.tests
  ]
