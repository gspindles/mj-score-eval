module Game.Mahjong.Test.Meld ( tests ) where

import Game.Mahjong.Class
import Game.Mahjong.Tile
import Game.Mahjong.Static.Tiles
import Game.Mahjong.Meld
import Game.Mahjong.Static.Melds

import Data.Maybe (fromJust)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit

instance Arbitrary Status where
  arbitrary = elements [Concealed, Revealed, Promoted]

tests :: TestTree
tests = testGroup "Game.Mahjong.Meld Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [
     classTests
   , constructionTests
   , predicateTests
   , utilityTests
   ]


-- | Class tests

classTests :: TestTree
classTests = testGroup "Class Tests" [
    showTests
  , eqTests
  , prettyTests
  , tilePredTests
  , cycleTests
  ]

showTests :: TestTree
showTests = testGroup "Show Tests" [
    testCase "Test Show Class" $
      show b111 @?= "Meld {status = Revealed, meldType = Triplet, meldTiles = [Tile Bamboo One,Tile Bamboo One,Tile Bamboo One]}"
  ]

eqTests :: TestTree
eqTests = testGroup "Eq Tests" [
    testCase "Test Eq Class" $
      c123 @?= c123
  ]

prettyTests :: TestTree
prettyTests = testGroup "Pretty Tests" [
    testCase "Pretty print sequences" $
      pp c123 @?= "+<C1 C2 C3>"
  , testCase "Pretty print triplets" $
      pp b111 @?= "+[B1 B1 B1]"
  , testCase "Pretty print quartets" $
      pp k1111 @?= "+{K1 K1 K1 K1}"
  , testCase "Pretty print pairs" $
      pp dww @?= "+(DW DW)"
  , testCase "Pretty print concealed" $
      pp (fromJust $ mkMeld Concealed Sequence [c1, c2, c3]) @?= "-<C1 C2 C3>"
  , testCase "Pretty print revealed" $
      pp (fromJust $ mkMeld Revealed Triplet [wn, wn, wn]) @?= "+[WN WN WN]"
  , testCase "Pretty print promoted" $
      pp (fromJust $ mkMeld Promoted Quartet [dw, dw, dw, dw]) @?= "^{DW DW DW DW}"
  ]

tilePredTests :: TestTree
tilePredTests = testGroup "TilePred Tests" [
    testCase "TilePred isCoin Test" . true $
      all isCoin coinMelds
  , testCase "TilePred not isCoin Test" . true $
      all (not . isCoin) . concat $ [bambooMelds, characterMelds, honorMelds]
  , testCase "TilePred isBamboo Test" . true $
      all isBamboo bambooMelds
  , testCase "TilePred not isBamboo Test" . true $
      all (not . isBamboo) . concat $ [coinMelds, characterMelds, honorMelds]
  , testCase "TilePred isCharacter Test" . true $
      all isCharacter characterMelds
  , testCase "TilePred not isCharacter Test" . true $
      all (not . isCharacter) . concat $ [coinMelds, bambooMelds, honorMelds]
  , testCase "TilePred isSimple Test" . true $
      all isSimple [c234, b555, k6666, c88]
  , testCase "TilePred not isSimple Test" . true $
      all (not . isSimple) . concat $ [[c123, b111, k9999, b99], honorMelds]
  , testCase "TilePred isTeminal Test" . true $
      all isTerminal [c123, b111, k9999, b99]
  , testCase "TilePred not isTeminal Test" . true $
      all (not . isTerminal) . concat $ [[c234, b555, k6666, c88], honorMelds]
  , testCase "TilePred isSuit Test" . true $
      all isSuit suitMelds
  , testCase "TilePred not isSuit Test" . true $
      all (not . isSuit) honorMelds
  , testCase "TilePred isWind Test" . true $
      all isWind windMelds
  , testCase "TilePred not isWind Test" . true $
      all (not . isWind) . concat $ [suitMelds, dragonMelds]
  , testCase "TilePred isDragon Test" . true $
      all isDragon dragonMelds
  , testCase "TilePred not isDragon Test" . true $
      all (not . isDragon) . concat $ [suitMelds, windMelds]
  , testCase "TilePred isHonor Test" . true $
      all isHonor honorMelds
  , testCase "TilePred not isHonor Test" . true $
      all (not . isHonor) suitMelds
  , testCase "TilePred isEdge Test" . true $
      all isEdge . concat $ [[c123, b111, k1111, c11], windTriplets, dragonQuartets, dragonPairs]
  , testCase "TilePred not isEdge Test" . true $
      all (not . isEdge) [c234, b555, k8888]
  , testCase "TilePred isGreen Test" . true $
      all isGreen [b234, b6666, b88, dgg, dggg, dgggg]
  , testCase "TilePred not isGreen Test" . true $
      all (not . isGreen) . concat $ [coinMelds, characterMelds, [b111, b5555, b777, b99, drr, drrr, dwwww], windMelds]
  , testCase "TilePred isRed Test" . true $
      all isRed [b111, b5555, b777, b99, drr, drrr, drrrr]
  , testCase "TilePred not isRed Test" . true $
      all (not . isRed) . concat $ [coinMelds, characterMelds, [b234, b6666, b88, dgg, dggg, dgggg], windMelds]
  ]

cycleTests :: TestTree
cycleTests = testGroup "Cycle Tests" [
    testCase "Test next sequence" $
      next c123 @?= c234
  , testCase "Test previous sequence" $
      prev c234 @?= c123
  , testCase "Test next triplet" $
      next c111 @?= c222
  , testCase "Test previous triplet" $
      prev c111 @?= c999
  , testCase "Test next quartet" $
      next weeee @?= wssss
  , testCase "Test previous quartet" $
      prev drrrr @?= dwwww
  , testCase "Test next eyes" $
      next c66 @?= c77
  , testCase "Test previous eyes" $
      prev c11 @?= c99
  ]


-- Construction tests

constructionTests :: TestTree
constructionTests = testGroup "Construction Tests" [
    mkSequenceTests
  , mkTripletTests
  , mkQuartetTests
  , mkPairTests
  , mkMeldTests
  , promotionTests
  ]

mkSequenceTests :: TestTree
mkSequenceTests = testGroup "Sequence creation tests" [
    testCase "Sequence fails with Promote status" $
      mkSequence Promoted [c1, c2, c3] @?= Nothing
  , testCase "Sequence fails with wrong sequence length" $
      mkSequence Revealed [c1, b2, c3, c4] @?= Nothing
  , testCase "Sequence fails with bonus tile" $
      mkSequence Revealed [c1, c2, f4] @?= Nothing
  , testCase "Sequence fails with honor tile" $
      mkSequence Revealed [c1, ww, dr] @?= Nothing
  , testCase "Sequence fails with multiple suits" $
      mkSequence Revealed [c1, b2, k3] @?= Nothing
  , testCase "Sequence fails with tiles not in sequence" $
      mkSequence Revealed [c1, c2, c4] @?= Nothing
  , testCase "Sequence passes" $
      mkSequence Revealed [c1, c2, c3] @?= Just c123
  ]

mkTripletTests :: TestTree
mkTripletTests = testGroup "Triplet creation tests" [
    testCase "Triplet fails with Promote status" $
      mkTriplet Promoted [c1, c1, c1] @?= Nothing
  , testCase "Triplet fails with wrong sequence length" $
      mkTriplet Revealed [c1, c1, c1, c1] @?= Nothing
  , testCase "Triplet fails with bonus tile" $
      mkTriplet Revealed [c1, c1, f1] @?= Nothing
  , testCase "Triplet fails with multiple suits" $
      mkTriplet Revealed [c1, b1, k1] @?= Nothing
  , testCase "Triplet fails with tiles not same" $
      mkTriplet Revealed [c1, c1, c2] @?= Nothing
  , testCase "Triplet succeeds" $
      mkTriplet Revealed [c1, c1, c1] @?= Just c111
  , testCase "Triplet succeeds" $
      mkTriplet Revealed [wn, wn, wn] @?= Just wnnn
  ]

mkQuartetTests :: TestTree
mkQuartetTests = testGroup "Quartet creation tests" [
    testCase "Quartet passes with Promote status" $
      mkQuartet Promoted [c1, c1, c1, c1] @?= promoteTriplet c111 c1
  , testCase "Quartet fails with wrong sequence length" $
      mkQuartet Revealed [c1, c1, c1] @?= Nothing
  , testCase "Quartet fails with bonus tile" $
      mkQuartet Revealed [c1, c1, c1, f1] @?= Nothing
  , testCase "Quartet fails with multiple suits" $
      mkQuartet Revealed [c1, c1, c1, k1] @?= Nothing
  , testCase "Quartet fails with tiles not same" $
      mkQuartet Revealed [c1, c1, c1, c2] @?= Nothing
  , testCase "Quartet succeeds" $
      mkQuartet Revealed [c1, c1, c1, c1] @?= Just c1111
  , testCase "Quartet succeeds" $
      mkQuartet Revealed [wn, wn, wn, wn] @?= Just wnnnn
  ]

mkPairTests :: TestTree
mkPairTests = testGroup "Pair creation tests" [
    testCase "Pair fails with Promote status" $
      mkPair Promoted [c1, c1] @?= Nothing
  , testCase "Pair fails with wrong sequence length" $
      mkPair Revealed [c1, c1, c1] @?= Nothing
  , testCase "Pair fails with bonus tile" $
      mkPair Revealed [c1, f1] @?= Nothing
  , testCase "Pair fails with multiple suits" $
      mkPair Revealed [c1, b1] @?= Nothing
  , testCase "Pair fails with tiles not same" $
      mkPair Revealed [c1, c2] @?= Nothing
  , testCase "Pair succeeds" $
      mkPair Revealed [c1, c1] @?= Just c11
  , testCase "Pair succeeds" $
      mkPair Revealed [wn, wn] @?= Just wnn
  ]

mkMeldTests :: TestTree
mkMeldTests = testGroup "Meld creation tests" [
    testCase "Create Sequence meld" $
      mkMeld Concealed Sequence [c1, c2, c3] @?= mkSequence Concealed [c1, c2, c3]
  , testCase "Create Triplet meld" $
      mkMeld Revealed Triplet [dr, dr, dr] @?= mkTriplet Revealed [dr, dr, dr]
  , testCase "Create Quartet meld" $
      mkMeld Revealed Quartet [ww, ww, ww, ww] @?= mkQuartet Revealed [ww, ww, ww, ww]
  , testCase "Create Pair meld" $
      mkMeld Concealed Pair [b1, b1] @?= mkPair Concealed [b1, b1]
  ]

promotionTests :: TestTree
promotionTests = testGroup "Promotion tests" [
    testCase "Promote Sequence fails" $
      (mkSequence Revealed [c1, c2, c3] >>= flip promoteTriplet c1) @?= Nothing
  , testCase "Promote Triplet fails when concealed" $
      (mkTriplet Concealed [dr, dr, dr] >>= flip promoteTriplet dr) @?= Nothing
  , testCase "Promote Triplet fails when tile mismatch" $
      (mkTriplet Revealed [dr, dr, dr] >>= flip promoteTriplet dg) @?= Nothing
  , testCase "Promote Triplet passes" $
      (mkTriplet Revealed [dr, dr, dr] >>= flip promoteTriplet dr) @?= mkQuartet Promoted [dr, dr, dr, dr]
  , testCase "Promote Quartet fails" $
      (mkQuartet Revealed [b1, b1, b1, b1] >>= flip promoteTriplet b1) @?= Nothing
  , testCase "Promote Pair fails" $
      (mkPair Revealed [b1, b1] >>= flip promoteTriplet b1) @?= Nothing
  ]


-- predicate tests
predicateTests :: TestTree
predicateTests = testGroup "Predicate tests" $ [
    testCase "isConcealed Test" . true $
      isConcealed . fromJust $ mkSequence Concealed [c1, c2, c3]
  , testCase "isConcealed Test" . false $
      isConcealed . fromJust $ mkTriplet Revealed [ws, ws, ws]
  , testCase "isConcealed Test" . false $
      isConcealed . fromJust $ mkQuartet Promoted [dr, dr, dr, dr]
  , testCase "isConcealed Test" . true $
      isConcealed . fromJust $ mkPair Concealed [c1, c1]
  , testCase "isRevealed Test" . false $
      isRevealed . fromJust $ mkSequence Concealed [b1, b2, b3]
  , testCase "isRevealed Test" . true $
      isRevealed . fromJust $ mkTriplet Revealed [ww, ww, ww]
  , testCase "isRevealed Test" . true $
      isRevealed . fromJust $ mkQuartet Promoted [dw, dw, dw, dw]
  , testCase "isRevealed Test" . false $
      isRevealed . fromJust $ mkPair Concealed [b1, b1]
  , testCase "isPromoted Test" . false $
      isPromoted . fromJust $ mkSequence Concealed [k1, k2, k3]
  , testCase "isPromoted Test" . false $
      isPromoted . fromJust $ mkTriplet Revealed [we, we, we]
  , testCase "isPromoted Test" . true $
      isPromoted . fromJust $ mkQuartet Promoted [dg, dg, dg, dg]
  , testCase "isPromoted Test" . false $
      isPromoted . fromJust $ mkPair Revealed [k1, k1]
  , testCase "isSequence Test" . true $
      isSequence c123
  , testCase "isSequence Test" . false $
      isSequence $ wnnn
  , testCase "isSequence Test" . false $
      isSequence $ drrrr
  , testCase "isSequence Test" . false $
      isSequence $ c11
  , testCase "isTriplet Test" . false $
      isTriplet $ b123
  , testCase "isTriplet Test" . true $
      isTriplet weee
  , testCase "isTriplet Test" . true $
      isTriplet dgggg
  , testCase "isTriplet Test" . false $
      isTriplet $ b11
  , testCase "isQuartet Test" . false $
      isQuartet $ k123
  , testCase "isQuartet Test" . false $
      isQuartet $ wwww
  , testCase "isQuartet Test" . true $
      isQuartet dwwww
  , testCase "isQuartet Test" . false $
      isQuartet $ k11
  , testCase "isPair Test" . false $
      isPair $ k123
  , testCase "isPair Test" . false $
      isPair $ wsss
  , testCase "isPair Test" . false $
      isPair $ dwwww
  , testCase "isPair Test" . true $
      isPair k11
  ]


-- Utility tests

utilityTests :: TestTree
utilityTests = testGroup "Function tests" $ [
    testCase "meldTileMatch False fails comparing different meld type" . false $
      meldTileMatch False (fromJust $ mkSequence Concealed [c1, c2, c3])
                          (fromJust $ mkTriplet Concealed [b1, b1, b1])
  , testCase "meldTileMatch False fails comparing different meld tile" . false $
      meldTileMatch False (fromJust $ mkSequence Concealed [c1, c2, c3])
                          (fromJust $ mkSequence Concealed [b1, b2, b3])
  , testCase "meldTileMatch False passes comparing same meld different status" . true $
      meldTileMatch False (fromJust $ mkSequence Concealed [c1, c2, c3])
                          (fromJust $ mkSequence Revealed [c1, c2, c3])
  , testCase "meldTileMatch False passes comparing same meld same status" . true $
      meldTileMatch False (fromJust $ mkSequence Concealed [c1, c2, c3])
                          (fromJust $ mkSequence Concealed [c1, c2, c3])
  , testCase "meldTileMatch False fails comparing Triplet and Quartet" . false $
      meldTileMatch False (fromJust $ mkTriplet Concealed [c1, c1, c1])
                          (fromJust $ mkQuartet Revealed [c1, c1, c1, c1])
  , testCase "meldTileMatch True fails comparing different meld type" . false $
      meldTileMatch True  (fromJust $ mkSequence Concealed [c1, c2, c3])
                          (fromJust $ mkTriplet Concealed [b1, b1, b1])
  , testCase "meldTileMatch True fails comparing different meld tiles" . false $
      meldTileMatch True  (fromJust $ mkSequence Concealed [c1, c2, c3])
                          (fromJust $ mkSequence Concealed [b1, b2, b3])
  , testCase "meldTileMatch True passes comparing same meld different status" . true $
      meldTileMatch True  (fromJust $ mkSequence Concealed [c1, c2, c3])
                          (fromJust $ mkSequence Revealed [c1, c2, c3])
  , testCase "meldTileMatch True passes comparing same meld same status" . true $
      meldTileMatch True  (fromJust $ mkSequence Concealed [c1, c2, c3])
                          (fromJust $ mkSequence Concealed [c1, c2, c3])
  , testCase "meldTileMatch True passes comparing Triplet and Quartet" . true $
      meldTileMatch True  (fromJust $ mkTriplet Concealed [c1, c1, c1])
                          (fromJust $ mkQuartet Revealed [c1, c1, c1, c1])
  ]

true = (@=?) True
false = (@=?) False

