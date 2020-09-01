module Game.Mahjong.Test.Tile ( tests ) where

import Game.Mahjong.Class
import Game.Mahjong.Tile
import Game.Mahjong.Static.Tiles

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit

instance Arbitrary Tile where
  arbitrary = elements allTiles

tests :: TestTree
tests = testGroup "Game.Mahjong.Tile Tests" [
    classTests
  , utilityTests
  , constructionTests
  ]

-- | Class tests

classTests :: TestTree
classTests = testGroup "Class Tests" [
    showTests
  , eqTests
  , ordTests
  , prettyTests
  , tilePredTests
  , cycleTests
  ]

showTests :: TestTree
showTests = testGroup "Show Tests" [
    testCase "Test Show Class" $
      show c1 @?= "Tile Coin One"
  ]

eqTests :: TestTree
eqTests = testGroup "Eq Tests" [
    testCase "Test Eq Class" $
      c1 @?= c1
  ]

ordTests :: TestTree
ordTests = testGroup "Ord Tests" [
    testCase "LT Test" $
      compare c1 c2 @?= LT
  , testCase "EQ Test" $
      compare c1 c1 @?= EQ
  , testCase "GT Test" $
      compare b1 c1 @?= GT
  ]

prettyTests :: TestTree
prettyTests = testGroup "Pretty Tests" [
    testCase "Pretty Test coin" $
      pp c1 @?= "C1"
  , testCase "Pretty Test bamboo" $
      pp b2 @?= "B2"
  , testCase "Pretty Test character" $
      pp k9 @?= "K9"
  , testCase "Pretty Test wind" $
      pp we @?= "WE"
  , testCase "Pretty Test dragon" $
      pp dw @?= "DW"
  , testCase "Pretty Test flower" $
      pp f1 @?= "F1"
  , testCase "Pretty Test season" $
      pp s4 @?= "S4"
  ]

tilePredTests :: TestTree
tilePredTests = testGroup "TilePred Tests" [
    testCase "TilePred isCoin Test" . true $
      all isCoin coins
  , testCase "TilePred not isCoin Test" . true $
      all (not . isCoin) . concat $ [bamboos, characters, honors, bonuses]
  , testCase "TilePred isBamboo Test" . true $
      all isBamboo bamboos
  , testCase "TilePred not isBamboo Test" . true $
      all (not . isBamboo) . concat $ [coins, characters, honors, bonuses]
  , testCase "TilePred isCharacter Test" . true $
      all isCharacter characters
  , testCase "TilePred not isCharacter Test" . true $
      all (not . isCharacter) . concat $ [coins, bamboos, honors, bonuses]
  , testCase "TilePred isSimple Test" . true $
      all isSimple simples
  , testCase "TilePred not isSimple Test" . true $
      all (not . isSimple) . concat $ [terminals, honors, bonuses]
  , testCase "TilePred isTeminal Test" . true $
      all isTerminal terminals
  , testCase "TilePred not isTeminal Test" . true $
      all (not . isTerminal) . concat $ [simples, honors, bonuses]
  , testCase "TilePred isSuit Test" . true $
      all isSuit suits
  , testCase "TilePred not isSuit Test" . true $
      all (not . isSuit) . concat $ [honors, bonuses]
  , testCase "TilePred isWind Test" . true $
      all isWind winds
  , testCase "TilePred not isWind Test" . true $
      all (not . isWind) . concat $ [suits, dragons, bonuses]
  , testCase "TilePred isDragon Test" . true $
      all isDragon dragons
  , testCase "TilePred not isDragon Test" . true $
      all (not . isDragon) . concat $ [suits, winds, bonuses]
  , testCase "TilePred isHonor Test" . true $
      all isHonor honors
  , testCase "TilePred not isHonor Test" . true $
      all (not . isHonor) . concat $ [suits, bonuses]
  , testCase "TilePred isEdge Test" . true $
      all isEdge edges
  , testCase "TilePred not isEdge Test" . true $
      all (not . isEdge) . concat $ [simples, bonuses]
  , testCase "TilePred isFlower Test" . true $
      all isFlower flowers
  , testCase "TilePred not isFlower Test" . true $
      all (not . isFlower) . concat $ [suits, honors, seasons]
  , testCase "TilePred isSeason Test" . true $
      all isSeason seasons
  , testCase "TilePred not isSeason Test" . true $
      all (not . isSeason) . concat $ [suits, honors, flowers]
  , testCase "TilePred isBonus Test" . true $
      all isBonus bonuses
  , testCase "TilePred not isBonus Test" . true $
      all (not . isBonus) . concat $ [suits, honors]
  , testCase "TilePred isGreen Test" . true $
      all isGreen greens
  , testCase "TilePred not isGreen Test" . true $
      all (not . isGreen) . concat $ [coins, characters, [b1, b5, b7, b9], winds, [dr, dw], bonuses]
  , testCase "TilePred isRed Test" . true $
      all isRed reds
  , testCase "TilePred not isRed Test" . true $
      all (not . isRed) . concat $ [coins, characters, [b2, b3, b4, b6, b8], winds, [dg], bonuses]
  ]

cycleTests :: TestTree
cycleTests = testGroup "Cycle Tests" [
    testCase "Test next" $
      next c1 @?= c2
  , testCase "Test previous" $
      prev c1 @?= c9
  ]


-- Utility tests

utilityTests :: TestTree
utilityTests = testGroup "Utility Tests" [
    testCase "Test tileType" $
      tileType (mkCoin One) @?= Coin
  , testCase "Test tileValue" $
      tileValue (mkCoin One) @?= 1
  , testCase "Test isSameTileType" . true $
      all isSameTileType [coins, bamboos, characters, winds, dragons, flowers, seasons]
  ]


-- Construction tests

constructionTests :: TestTree
constructionTests = testGroup "Construction Tests" [
    testCase "Test mkCoin" $
      mkCoin One @?= c1
  , testCase "Test mkBamboo" $
      mkBamboo Two @?= b2
  , testCase "Test mkCharacter" $
      mkCharacter Three @?= k3
  , testCase "Test mkWind" $
      mkWind East @?= we
  , testCase "Test mkDragon" $
      mkDragon White @?= dw
  , testCase "Test mkFlower" $
      mkFlower BambooTree @?= f4
  , testCase "Test mkSeason" $
      mkSeason Winter @?= s4
  ]

true = (@=?) True

