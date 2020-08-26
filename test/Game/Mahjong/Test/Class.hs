module Game.Mahjong.Test.Class ( tests ) where

import Game.Mahjong.Class

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

tests :: TestTree
tests = testGroup "Game.Mahjong.Class Tests" [propertyTests]

propertyTests :: TestTree
propertyTests = testGroup "Properties tests" [qcPropertyTests]

qcPropertyTests :: TestTree
qcPropertyTests = testGroup "QuickCheck property tests" [
    QC.testProperty "fmap ($ x) fs == zipWith id fs . repeat" $
      \list int -> (fmap ($ (int :: Integer)) (list :: [Integer -> Integer]))
                == (zipWith id list . repeat $ int)
  , QC.testProperty "zipWith id fs . repeat == zipWith ($) fs . repeat" $
      \list int -> (zipWith id (list :: [Integer -> Integer]) $ repeat (int :: Integer))
                == (zipWith ($) list $ repeat int)
  ]

