module Game.Mahjong.Test.Static.Tiles ( tests ) where

import Game.Mahjong.Tile
import Game.Mahjong.Static.Tiles

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Game.Mahjong.Static.Tiles Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [collectionsTests, tileTests]

collectionsTests :: TestTree
collectionsTests = testGroup "collections count tests" [
    testCase "There are 9 coin tiles" $
      length coins @?= 9
  , testCase "There are 9 bamboo tiles" $
      length bamboos @?= 9
  , testCase "There are 9 character tiles" $
      length characters @?= 9
  , testCase "There are 4 wind tiles" $
      length winds @?= 4
  , testCase "There are 3 dragon tiles" $
      length dragons @?= 3
  , testCase "There are 4 flower tiles" $
      length flowers @?= 4
  , testCase "There are 4 season tiles" $
      length seasons @?= 4
  , testCase "There are 21 simple tiles" $
      length simples @?= 3 * (9 - 2)
  , testCase "There are 6 terminal tiles" $
      length terminals @?= 3 * 2
  , testCase "There are 27 suit tiles" $
      length suits @?= 3 * 9
  , testCase "There are 7 honor tiles" $
      length honors @?= 4 + 3
  , testCase "There are 13 edge tiles" $
      length edges @?= 6 + 7
  , testCase "There are 8 bonus tiles" $
      length bonuses @?= 4 + 4
  , testCase "There are 6 green tiles" $
      length greens @?= 5 + 1
  , testCase "There are 5 red tiles" $
      length reds @?= 4 + 1
  , testCase "There are 34 regular tiles" $
      length regulars @?= 3 * 9 + 4 + 3
  , testCase "There are 42 all tiles" $
      length allTiles @?= 3 * 9 + 4 + 3 + 4 * 2
--, testCase "There are 144 tiles in a wall" $
--    length mjSet @?= 4 * (3 * 9 + 4 + 3) + 2 * 4
  ]

tileTests :: TestTree
tileTests = testGroup "tile tests" [
    testCase "tileType test" $
      fmap tileType allTiles @?= tileTypes
  , testCase "tileValue test" $
      fmap tileValue allTiles @?= tileValues
  ]
  where
    tileTypes  = concatMap (replicate 9) [Coin, Bamboo, Character]
              ++ replicate 4 Wind ++ replicate 3 Dragon
              ++ concatMap (replicate 4) [Flower, Season]
    tileValues = mconcat (replicate 3 [1..9])
              ++ [1..4] ++ [1..3]
              ++ mconcat (replicate 3 [1..4])

