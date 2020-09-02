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
classTests = testGroup "Class tests" [
    showTests
  , eqTests
  , ordTests
  , prettyTests
  , tilePredTests
  , cycleTests
  ]

showTests :: TestTree
showTests = testGroup "Show tests" [
    testCase "Show show" $
      show c1 @?= "Tile Coin One"
  ]

eqTests :: TestTree
eqTests = testGroup "Eq tests" [
    testCase "Eq equal" $
      c1 @?= c1
  ]

ordTests :: TestTree
ordTests = testGroup "Ord tests" [
    testCase "Ord LT" $
      compare c1 c2 @?= LT
  , testCase "Ord EQ" $
      compare c1 c1 @?= EQ
  , testCase "Ord GT" $
      compare b1 c1 @?= GT
  ]

prettyTests :: TestTree
prettyTests = testGroup "Pretty tests" [
    testCase "Pretty coin tile" $
      pp c1 @?= "C1"
  , testCase "Pretty bamboo tile" $
      pp b2 @?= "B2"
  , testCase "Pretty character tile" $
      pp k9 @?= "K9"
  , testCase "Pretty wind tile" $
      pp we @?= "WE"
  , testCase "Pretty dragon tile" $
      pp dw @?= "DW"
  , testCase "Pretty flower tile" $
      pp f1 @?= "F1"
  , testCase "Pretty season tile" $
      pp s4 @?= "S4"
  ]

tilePredTests :: TestTree
tilePredTests = testGroup "TilePred tests" [
    testCase "TilePred isCoin" . true $
      all isCoin coins
  , testCase "TilePred not isCoin" . true $
      all (not . isCoin) . concat $ [bamboos, characters, honors, bonuses]
  , testCase "TilePred isBamboo" . true $
      all isBamboo bamboos
  , testCase "TilePred not isBamboo" . true $
      all (not . isBamboo) . concat $ [coins, characters, honors, bonuses]
  , testCase "TilePred isCharacter" . true $
      all isCharacter characters
  , testCase "TilePred not isCharacter" . true $
      all (not . isCharacter) . concat $ [coins, bamboos, honors, bonuses]
  , testCase "TilePred isSimple" . true $
      all isSimple simples
  , testCase "TilePred not isSimple" . true $
      all (not . isSimple) . concat $ [terminals, honors, bonuses]
  , testCase "TilePred isTeminal" . true $
      all isTerminal terminals
  , testCase "TilePred not isTeminal" . true $
      all (not . isTerminal) . concat $ [simples, honors, bonuses]
  , testCase "TilePred isSuitTest" . true $
      all isSuit suits
  , testCase "TilePred not isSuit" . true $
      all (not . isSuit) . concat $ [honors, bonuses]
  , testCase "TilePred isWind" . true $
      all isWind winds
  , testCase "TilePred not isWind" . true $
      all (not . isWind) . concat $ [suits, dragons, bonuses]
  , testCase "TilePred isDragon" . true $
      all isDragon dragons
  , testCase "TilePred not isDragon" . true $
      all (not . isDragon) . concat $ [suits, winds, bonuses]
  , testCase "TilePred isHonor" . true $
      all isHonor honors
  , testCase "TilePred not isHonor" . true $
      all (not . isHonor) . concat $ [suits, bonuses]
  , testCase "TilePred isEdge" . true $
      all isEdge edges
  , testCase "TilePred not isEdge" . true $
      all (not . isEdge) . concat $ [simples, bonuses]
  , testCase "TilePred isFlower" . true $
      all isFlower flowers
  , testCase "TilePred not isFlower" . true $
      all (not . isFlower) . concat $ [suits, honors, seasons]
  , testCase "TilePred isSeason" . true $
      all isSeason seasons
  , testCase "TilePred not isSeason" . true $
      all (not . isSeason) . concat $ [suits, honors, flowers]
  , testCase "TilePred isBonus" . true $
      all isBonus bonuses
  , testCase "TilePred not isBonus" . true $
      all (not . isBonus) . concat $ [suits, honors]
  , testCase "TilePred isGreen" . true $
      all isGreen greens
  , testCase "TilePred not isGreen" . true $
      all (not . isGreen) . concat $ [coins, characters, [b1, b5, b7, b9], winds, [dr, dw], bonuses]
  , testCase "TilePred isRed" . true $
      all isRed reds
  , testCase "TilePred not isRed" . true $
      all (not . isRed) . concat $ [coins, characters, [b2, b3, b4, b6, b8], winds, [dg], bonuses]
  ]

cycleTests :: TestTree
cycleTests = testGroup "Cycle tests" [
    testCase "Cycle next" $
      next c1 @?= c2
  , testCase "Cycle previous" $
      prev c1 @?= c9
  ]


-- | Utility tests

utilityTests :: TestTree
utilityTests = testGroup "Utility tests" [
    testCase "tileType" $
      tileType (mkCoin One) @?= Coin
  , testCase "tileValue" $
      tileValue (mkCoin One) @?= 1
  , testCase "isSameTileType" . true $
      all isSameTileType [coins, bamboos, characters, winds, dragons, flowers, seasons]
  ]


-- | Construction tests

constructionTests :: TestTree
constructionTests = testGroup "Construction tests" [
    testCase "mkCoin" $
      mkCoin One @?= c1
  , testCase "mkBamboo" $
      mkBamboo Two @?= b2
  , testCase "mkCharacter" $
      mkCharacter Three @?= k3
  , testCase "mkWind" $
      mkWind East @?= we
  , testCase "mkDragon" $
      mkDragon White @?= dw
  , testCase "mkFlower" $
      mkFlower BambooTree @?= f4
  , testCase "mkSeason" $
      mkSeason Winter @?= s4
  ]

true = (@=?) True

