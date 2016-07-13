module Game.Mahjong.Test.Meld ( tests ) where

import Game.Mahjong.Tile
import Game.Mahjong.Meld

import Data.Maybe (fromJust)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit

instance Arbitrary Status where
  arbitrary = elements [Concealed, Revealed, Promoted]

tests :: TestTree
tests = testGroup "Game.Mahjong.Meld Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [mkTests, functionTests]

mkTests :: TestTree
mkTests = testGroup "Meld creation tests" [
    mkChowTests, mkPungTests, mkKongTests, mkEyesTests
  , mkMeldTests, promotionTests
  ]

mkChowTests :: TestTree
mkChowTests = testGroup "Chow creation tests" [
    testCase "Chow fails with Promote status" $
      mkChow Promoted [c1, c2, c3] @?= Nothing
  , testCase "Chow fails with wrong sequence length" $
      mkChow Revealed [c1, b2, c3, c4] @?= Nothing
  , testCase "Chow fails with bonus tile" $
      mkChow Revealed [c1, c2, f4] @?= Nothing
  , testCase "Chow fails with honor tile" $
      mkChow Revealed [c1, ww, dr] @?= Nothing
  , testCase "Chow fails with multiple suits" $
      mkChow Revealed [c1, b2, k3] @?= Nothing
  , testCase "Chow fails with tiles not in sequence" $
      mkChow Revealed [c1, c2, c4] @?= Nothing
  , testCase "Chow passes" $
      mkChow Revealed [c1, c2, c3] @?= Just c123
  ]

mkPungTests :: TestTree
mkPungTests = testGroup "Pung creation tests" [
    testCase "Pung fails with Promote status" $
      mkPung Promoted [c1, c1, c1] @?= Nothing
  , testCase "Pung fails with wrong sequence length" $
      mkPung Revealed [c1, c1, c1, c1] @?= Nothing
  , testCase "Pung fails with bonus tile" $
      mkPung Revealed [c1, c1, f1] @?= Nothing
  , testCase "Pung fails with multiple suits" $
      mkPung Revealed [c1, b1, k1] @?= Nothing
  , testCase "Pung fails with tiles not same" $
      mkPung Revealed [c1, c1, c2] @?= Nothing
  , testCase "Pung succeeds" $
      mkPung Revealed [c1, c1, c1] @?= Just c111
  , testCase "Pung succeeds" $
      mkPung Revealed [wn, wn, wn] @?= Just wnnn
  ]

mkKongTests :: TestTree
mkKongTests = testGroup "Kong creation tests" [
    testCase "Kong passes with Promote status" $
      mkKong Promoted [c1, c1, c1, c1] @?= promotePung c111 c1
  , testCase "Kong fails with wrong sequence length" $
      mkKong Revealed [c1, c1, c1] @?= Nothing
  , testCase "Kong fails with bonus tile" $
      mkKong Revealed [c1, c1, c1, f1] @?= Nothing
  , testCase "Kong fails with multiple suits" $
      mkKong Revealed [c1, c1, c1, k1] @?= Nothing
  , testCase "Kong fails with tiles not same" $
      mkKong Revealed [c1, c1, c1, c2] @?= Nothing
  , testCase "Kong succeeds" $
      mkKong Revealed [c1, c1, c1, c1] @?= Just c1111
  , testCase "Kong succeeds" $
      mkKong Revealed [wn, wn, wn, wn] @?= Just wnnnn
  ]

mkEyesTests :: TestTree
mkEyesTests = testGroup "Eyes creation tests" [
    testCase "Eyes fails with Promote status" $
      mkEyes Promoted [c1, c1] @?= Nothing
  , testCase "Eyes fails with wrong sequence length" $
      mkEyes Revealed [c1, c1, c1] @?= Nothing
  , testCase "Eyes fails with bonus tile" $
      mkEyes Revealed [c1, f1] @?= Nothing
  , testCase "Eyes fails with multiple suits" $
      mkEyes Revealed [c1, b1] @?= Nothing
  , testCase "Eyes fails with tiles not same" $
      mkEyes Revealed [c1, c2] @?= Nothing
  , testCase "Eyes succeeds" $
      mkEyes Revealed [c1, c1] @?= Just c11
  , testCase "Eyes succeeds" $
      mkEyes Revealed [wn, wn] @?= Just wnn
  ]

mkMeldTests :: TestTree
mkMeldTests = testGroup "Meld creation tests" [
    testCase "Create chow meld" $
      mkMeld Concealed Chow [c1, c2, c3] @?= mkChow Concealed [c1, c2, c3]
  , testCase "Create Pung meld" $
      mkMeld Revealed Pung [dr, dr, dr] @?= mkPung Revealed [dr, dr, dr]
  , testCase "Create Kong meld" $
      mkMeld Revealed Kong [ww, ww, ww, ww] @?= mkKong Revealed [ww, ww, ww, ww]
  , testCase "Create Eyes meld" $
      mkMeld Concealed Eyes [b1, b1] @?= mkEyes Concealed [b1, b1]
  ]

promotionTests :: TestTree
promotionTests = testGroup "Promotion tests" [
    testCase "Promote Chow fails" $
      (mkChow Revealed [c1, c2, c3] >>= flip promotePung c1) @?= Nothing
  , testCase "Promote Pung fails when concealed" $
      (mkPung Concealed [dr, dr, dr] >>= flip promotePung dr) @?= Nothing
  , testCase "Promote Pung fails when tile mismatch" $
      (mkPung Revealed [dr, dr, dr] >>= flip promotePung dg) @?= Nothing
  , testCase "Promote Pung passes" $
      (mkPung Revealed [dr, dr, dr] >>= flip promotePung dr) @?= mkKong Promoted [dr, dr, dr, dr]
  , testCase "Promote Kong fails" $
      (mkKong Revealed [b1, b1, b1, b1] >>= flip promotePung b1) @?= Nothing
  , testCase "Promote Eyes fails" $
      (mkEyes Revealed [b1, b1] >>= flip promotePung b1) @?= Nothing
  ]

functionTests :: TestTree
functionTests = testGroup "Function tests" $ [
    testCase "meldTileMatch False fails comparing different meld type" . false $
      meldTileMatch False (fromJust $ mkChow Concealed [c1, c2, c3])
                          (fromJust $ mkPung Concealed [b1, b1, b1])
  , testCase "meldTileMatch False fails comparing different meld tile" . false $
      meldTileMatch False (fromJust $ mkChow Concealed [c1, c2, c3])
                          (fromJust $ mkChow Concealed [b1, b2, b3])
  , testCase "meldTileMatch False passes comparing same meld different status" . true $
      meldTileMatch False (fromJust $ mkChow Concealed [c1, c2, c3])
                          (fromJust $ mkChow Revealed [c1, c2, c3])
  , testCase "meldTileMatch False passes comparing same meld same status" . true $
      meldTileMatch False (fromJust $ mkChow Concealed [c1, c2, c3])
                          (fromJust $ mkChow Concealed [c1, c2, c3])
  , testCase "meldTileMatch False fails comparing Pung and Kong" . false $
      meldTileMatch False (fromJust $ mkPung Concealed [c1, c1, c1])
                          (fromJust $ mkKong Revealed [c1, c1, c1, c1])
  , testCase "meldTileMatch True fails comparing different meld type" . false $
      meldTileMatch True  (fromJust $ mkChow Concealed [c1, c2, c3])
                          (fromJust $ mkPung Concealed [b1, b1, b1])
  , testCase "meldTileMatch True fails comparing different meld tiles" . false $
      meldTileMatch True  (fromJust $ mkChow Concealed [c1, c2, c3])
                          (fromJust $ mkChow Concealed [b1, b2, b3])
  , testCase "meldTileMatch True passes comparing same meld different status" . true $
      meldTileMatch True  (fromJust $ mkChow Concealed [c1, c2, c3])
                          (fromJust $ mkChow Revealed [c1, c2, c3])
  , testCase "meldTileMatch True passes comparing same meld same status" . true $
      meldTileMatch True  (fromJust $ mkChow Concealed [c1, c2, c3])
                          (fromJust $ mkChow Concealed [c1, c2, c3])
  , testCase "meldTileMatch True passes comparing Pung and Kong" . true $
      meldTileMatch True  (fromJust $ mkPung Concealed [c1, c1, c1])
                          (fromJust $ mkKong Revealed [c1, c1, c1, c1])
  ]

true = (@=?) True
false = (@=?) False
