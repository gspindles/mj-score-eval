-- |
-- Module      :  Game.Mahjong.Example
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definition of tiles
--   along with tile related functions
module Game.Mahjong.Example

where

import Game.Mahjong.Hand
import Game.Mahjong.Internal.Meld
import Game.Mahjong.Tile


-------------------------------------------------------------------------------

{- Aliases -}

r = Revealed
c = Concealed


-------------------------------------------------------------------------------

{- Make a list of hands for testing -}

-- | 1.0 Trivial Patterns

chickenEx :: Hand
chickenEx = mkHand
  [ mkChow r c7
  , mkPung r k2
  , mkChow r k8
  , mkPung c b4
  ]
  (mkEyes r w3)
  [f1, s2]

allChowsEx :: Hand 
allChowsEx = mkHand
  [ mkChow r b4
  , mkChow r c7
  , mkChow r b2
  , mkChow c c1
  ]
  (mkEyes c d1)
  [f1, s2]

concealedEx :: Hand 
concealedEx = mkHand
  [ mkChow c c7
  , mkPung c w1
  , mkChow c k1
  , mkChow c b4
  ]
  (mkEyes r w3)
  [f1, s2]

selfDrawnEx :: Hand
selfDrawnEx = mkHand
  [ mkChow c c7
  , mkPung r w1
  , mkChow r k1
  , mkEyes c w3
  ]
  (mkChow c b4)
  [f1, s2]

allSimpleHandEx1 :: Hand
allSimpleHandEx1 = mkHand
  [ mkChow r b4
  , mkChow r c6
  , mkPung c b3
  , mkEyes c b2
  ]
  (mkPung r k6)
  []

allSimpleHandEx2 :: Hand
allSimpleHandEx2 = mkHand
  [ mkEyes c c2
  , mkEyes c c5
  , mkEyes c c8
  , mkEyes c b3
  , mkEyes c b5
  , mkEyes c k7
  ]
  (mkEyes r k8)
  [s3]

allTypesEx :: Hand
allTypesEx = mkHand
  [ mkChow r c7
  , mkPung r w2
  , mkPung c k4
  , mkChow c b6
  ]
  (mkEyes r d3)
  [f1]


-- | 2.0 Identical Chows

twoIdenticalChowsEx :: Hand
twoIdenticalChowsEx = mkHand
  [ mkChow r b4
  , mkChow c c6
  , mkChow r c6
  , mkEyes c k3
  ]
  (mkPung r w1)
  [s1]

twoIdenticalChowsTwiceEx :: Hand
twoIdenticalChowsTwiceEx = mkHand
  [ mkChow r b4
  , mkChow c c6
  , mkChow r c6
  , mkEyes c k3
  ]
  (mkChow r b4)
  [s1]

threeIdenticalChowsEx :: Hand
threeIdenticalChowsEx = mkHand
  [ mkChow r b4
  , mkChow r b4
  , mkChow c c6
  , mkEyes c k3
  ]
  (mkChow r b4)
  [s1]

fourIdenticalChowsEx :: Hand
fourIdenticalChowsEx = mkHand
  [ mkChow r b4
  , mkChow r b4
  , mkChow c b4
  , mkEyes c k3
  ]
  (mkChow r b4)
  [s1]


-- | 3.0 Pungs and Kongs

allPungsEx :: Hand
allPungsEx = mkHand
  [ mkPung r b4
  , mkPung c d2
  , mkKong c k6
  , mkPung c d2
  ]
  (mkEyes r b3)
  [f3]

twoConcealedPungsEx :: Hand
twoConcealedPungsEx = mkHand
  [ mkPung c b4
  , mkPung c c3
  , mkChow r k2
  , mkEyes c w2
  ]
  (mkChow r k6)
  [s2]

threeConcealedPungsEx :: Hand
threeConcealedPungsEx = mkHand
  [ mkPung c b4
  , mkKong c c3
  , mkChow r k2
  , mkEyes c w2
  ]
  (mkPung c k6)
  [s2]

fourConcealedPunsgEx :: Hand
fourConcealedPunsgEx = mkHand
  [ mkPung c b4
  , mkPung c c3
  , mkPung c k2
  , mkEyes c w2
  ]
  (mkPung c k6)
  [s2]

oneKongEx :: Hand
oneKongEx = mkHand
  [ mkKong r c3
  , mkChow c b2
  , mkPung r b9
  , mkEyes c d2
  ]
  (mkChow r k2)
  []

twoKongsEx :: Hand
twoKongsEx = mkHand
  [ mkKong r c3
  , mkChow c b2
  , mkKong r b9
  , mkEyes c d2
  ]
  (mkChow r k2)
  []

threeKongsEx :: Hand
threeKongsEx = mkHand
  [ mkKong r c3
  , mkChow c b2
  , mkKong r b9
  , mkEyes c d2
  ]
  (mkKong r k2)
  []

fourKongsEx :: Hand
fourKongsEx = mkHand
  [ mkKong r c3
  , mkKong c b2
  , mkKong r b9
  , mkEyes c d2
  ]
  (mkKong r k2)
  []


-- | 4.0 Similar Sets

threeSimilarChowsEx :: Hand
threeSimilarChowsEx = mkHand
  [ mkChow r k3
  , mkChow r b3
  , mkPung r d3
  , mkEyes c b7
  ]
  (mkChow r c3)
  [s2]

littleThreeSimilarPungsEx :: Hand
littleThreeSimilarPungsEx = mkHand
  [ mkPung r c6
  , mkKong r k6
  , mkChow c c1
  , mkEyes r c6
  ]
  (mkPung r d2)
  [s4]

threeSimilarPungsEx :: Hand
threeSimilarPungsEx = mkHand
  [ mkPung r c6
  , mkKong r k6
  , mkChow c c1
  , mkPung r c6
  ]
  (mkEyes r d2)
  [s4]


-- | 5.0 Consecutive Sets

threeConsecutiveChowsEx1 :: Hand
threeConsecutiveChowsEx1 = mkHand
  [ mkChow r c1
  , mkChow r c2
  , mkChow c c3
  , mkPung c b8
  ]
  (mkEyes r d2)
  [s1]

threeConsecutiveChowsEx2 :: Hand
threeConsecutiveChowsEx2 = mkHand
  [ mkChow r c1
  , mkChow r c3
  , mkChow c c5
  , mkPung c b8
  ]
  (mkEyes r d2)
  [s1]

nineTileStraightEx :: Hand
nineTileStraightEx = mkHand
  [ mkChow r b1
  , mkChow r b4
  , mkPung c w3
  , mkEyes c k2
  ]
  (mkChow r b7)
  [f2]

threeConsecutiveChowsTwiceEx1 :: Hand
threeConsecutiveChowsTwiceEx1 = mkHand
  [ mkChow r c2
  , mkChow r c3
  , mkChow c c4
  , mkChow c c6
  ]
  (mkEyes r b2)
  [f1, f3, s2]

threeConsecutiveChowsTwiceEx2 :: Hand
threeConsecutiveChowsTwiceEx2 = mkHand
  [ mkChow r c2
  , mkChow r c4
  , mkChow c c5
  , mkChow c c6
  ]
  (mkEyes r k7)
  []

fourConsecutiveChowsEx1 :: Hand 
fourConsecutiveChowsEx1 = mkHand 
  [ mkChow r c1
  , mkChow r c2
  , mkChow c c3
  , mkChow c c4
  ]
  (mkEyes r b2)
  [f1]

fourConsecutiveChowsEx2 :: Hand 
fourConsecutiveChowsEx2 = mkHand 
  [ mkChow r c1
  , mkChow r c3
  , mkChow c c5
  , mkChow c b7
  ]
  (mkEyes r k2)
  [f4, s3]


threeConsecutivePungsHandEx :: Hand
threeConsecutivePungsHandEx = mkHand
  [ mkPung r k5
  , mkPung r k6
  , mkChow c c2
  , mkEyes c d2
  ]
  (mkPung r k7)
  []

fourConsecutivePungsEx :: Hand
fourConsecutivePungsEx = mkHand
  [ mkPung r k5
  , mkPung r k6
  , mkChow r k7
  , mkEyes c d2
  ]
  (mkPung r k8)
  []

threeMothersEx :: Hand
threeMothersEx = mkHand
  [ mkPung r k5
  , mkPung r k6
  , mkChow r k5
  , mkEyes c d2
  ]
  (mkPung r k7)
  []


-- | 6.0 Suit Patterns

mixedOneSuitHandEx1 :: Hand
mixedOneSuitHandEx1 = mkHand
  [ mkPung r k5
  , mkPung r w1
  , mkChow r k2
  , mkEyes c d2
  ]
  (mkChow r k7)
  []

mixedOneSuitHandEx2 :: Hand
mixedOneSuitHandEx2 = mkHand
  [ mkEyes c c2
  , mkEyes c c5
  , mkEyes c c7
  , mkEyes c c8
  , mkEyes c w3
  , mkEyes c w4
  ]
  (mkEyes r d1)
  [s3]

pureOneSuitHandEx1 :: Hand
pureOneSuitHandEx1 = mkHand
  [ mkPung r k1
  , mkChow r k3
  , mkChow r k4
  , mkEyes c k9
  ]
  (mkPung r k8)
  [f3]

pureOneSuitHandEx2 :: Hand
pureOneSuitHandEx2 = mkHand
  [ mkEyes c c1
  , mkEyes c c2
  , mkEyes c c4
  , mkEyes c c5
  , mkEyes c c7
  , mkEyes c c8
  ]
  (mkEyes r c9)
  [s3]

nineGatesEx :: Hand
nineGatesEx = mkSpecial
  [ mkWrap b1, mkWrap b1, mkWrap b1
  , mkWrap b2, mkWrap b3, mkWrap b4
  , mkWrap b5
  , mkWrap b6, mkWrap b7, mkWrap b8
  , mkWrap b9, mkWrap b9, mkWrap b9
  ]
  b5
  [f3]


-- | 7.0 Terminal Tiles

twoTailedTerminalChowsHandEx1 :: Hand
twoTailedTerminalChowsHandEx1 = mkHand
  [ mkChow r b1
  , mkChow r b8
  , mkPung c k3
  , mkEyes c w2
  ]
  (mkChow r c6)
  [s1]

twoTailedTerminalChowsHandEx2 :: Hand
twoTailedTerminalChowsHandEx2 = mkHand
  [ mkChow r b1
  , mkChow r b8
  , mkChow c k1
  , mkEyes c c3
  ]
  (mkChow r k9)
  [f2]

twoTailedTerminalPungsHandEx1 :: Hand
twoTailedTerminalPungsHandEx1 = mkHand
  [ mkPung r b1
  , mkPung r b9
  , mkPung c w1
  , mkEyes c c3
  ]
  (mkChow r k4)
  []


twoTailedTerminalPungsHandEx2 :: Hand
twoTailedTerminalPungsHandEx2 = mkHand
  [ mkPung r b1
  , mkPung r b9
  , mkPung c k1
  , mkEyes c c3
  ]
  (mkPung r k9)
  [s3]

twoTailedTerminalsEx1 :: Hand
twoTailedTerminalsEx1 = mkHand
  [ mkPung r b1
  , mkPung r b9
  , mkChow c b1
  , mkEyes c c3
  ]
  (mkChow r b9)
  [s1]

twoTailedTerminalsEx2 :: Hand
twoTailedTerminalsEx2 = mkHand
  [ mkPung r b1
  , mkPung r b9
  , mkChow c b1
  , mkEyes c b5
  ]
  (mkChow r b9)
  [s1]

littleBoundlessMountainHandEx1 :: Hand
littleBoundlessMountainHandEx1 = mkHand
  [ mkPung r b1
  , mkPung r b9
  , mkChow c b1
  , mkEyes c b5
  ]
  (mkChow r b9)
  [f1]

littleBoundlessMountainHandEx2 :: Hand
littleBoundlessMountainHandEx2 = mkHand
  [ mkChow r k1
  , mkChow r k1
  , mkChow c k1
  , mkEyes c k9
  ]
  (mkChow r k9)
  [f1]

bigBoundlessMountainHandEx1 :: Hand
bigBoundlessMountainHandEx1 = mkHand
  [ mkPung r k1
  , mkChow c k1
  , mkChow r k9
  , mkEyes c k9
  ]
  (mkChow r k9)
  [s4]

bigBoundlessMountainHandEx2 :: Hand
bigBoundlessMountainHandEx2 = mkHand
  [ mkChow r k1
  , mkChow c k1
  , mkChow r k9
  , mkEyes c k1
  ]
  (mkPung r k9)
  [f4]

mixedLesserTerminalEx :: Hand
mixedLesserTerminalEx = mkHand
  [ mkChow r c1
  , mkPung r c9
  , mkChow c b1
  , mkPung c k9
  ]
  (mkEyes r w2)
  []

pureLesserTerminalEx :: Hand
pureLesserTerminalEx = mkHand
  [ mkChow r c1
  , mkPung r c9
  , mkChow r b1
  , mkPung c k1
  ]
  (mkEyes c b9)
  [f3]

mixedGreaterTerminalHandEx1 :: Hand
mixedGreaterTerminalHandEx1 = mkHand
  [ mkPung r c1
  , mkPung r c9
  , mkPung r w1
  , mkPung c d1
  ]
  (mkEyes c b9)
  [f1, s2]

mixedGreaterTerminalHandEx2 :: Hand
mixedGreaterTerminalHandEx2 = mkHand
  [ mkEyes c c9
  , mkEyes c b9
  , mkEyes c k1
  , mkEyes c w2
  , mkEyes c w4
  , mkEyes c d2
  ]
  (mkEyes r w1)
  []

pureGreaterTerminalHandEx1 :: Hand
pureGreaterTerminalHandEx1 = mkHand
  [ mkPung r c1
  , mkPung r c9
  , mkPung r b1
  , mkKong c k1
  ]
  (mkEyes c b9)
  [s3]

pureGreaterTerminalHandEx2 :: Hand
pureGreaterTerminalHandEx2 = mkHand
  [ mkEyes c c1
  , mkEyes c c9
  , mkEyes c b1
  , mkEyes c b9
  , mkEyes c k1
  , mkEyes c k9
  ]
  (mkEyes r k1)
  []


-- | 8.0 Honor Tiles


windPungEx :: Hand
windPungEx = mkHand
  [ mkPung r c8
  , mkChow r b3
  , mkChow r k8
  , mkPung r w3
  ]
  (mkEyes c w2)
  [s2]

littleThreeWindsEx :: Hand
littleThreeWindsEx = mkHand
  [ mkPung r w2
  , mkPung r w4
  , mkChow r c3
  , mkKong c k7
  ]
  (mkEyes c w1)
  [f1, f3]

bigThreeWindsEx :: Hand
bigThreeWindsEx = mkHand
  [ mkPung r w2
  , mkPung r w4
  , mkChow r c3
  , mkEyes c k7
  ]
  (mkPung c w1)
  [s1]

littleFourWindsEx :: Hand
littleFourWindsEx = mkHand
  [ mkPung r w2
  , mkPung c w3
  , mkPung r w4
  , mkChow r c3
  ]
  (mkEyes c w1)
  [f3, s2]

bigFourWindsEx :: Hand
bigFourWindsEx = mkHand
  [ mkPung r w2
  , mkPung c w3
  , mkPung r w4
  , mkEyes c k7
  ]
  (mkPung r w1)
  [f4, s2]

dragonPungEx :: Hand
dragonPungEx = mkHand
  [ mkChow r c1
  , mkChow r k2
  , mkPung r d1
  , mkEyes c d3
  ]
  (mkPung r w3)
  []

littleThreeDragonsEx :: Hand
littleThreeDragonsEx = mkHand
  [ mkPung r d1
  , mkPung r d2
  , mkChow c b3
  , mkPung c k8
  ]
  (mkEyes r d3)
  [s3]

bigThreeDragonsEx :: Hand
bigThreeDragonsEx = mkHand
  [ mkPung r d1
  , mkPung r d2
  , mkChow c b3
  , mkEyes c k8
  ]
  (mkPung r d3)
  [f2]

allHonorsEx1 :: Hand
allHonorsEx1 = mkHand
  [ mkPung r w1
  , mkPung r w4
  , mkPung r d1
  , mkPung c d2
  ]
  (mkPung r w3)
  [s1]

allHonorsEx2 :: Hand
allHonorsEx2 = mkHand
  [ mkEyes c w1
  , mkEyes c w3
  , mkEyes c w3
  , mkEyes c w4
  , mkEyes c d1
  , mkEyes c d2
  ]
  (mkEyes r w1)
  [s3]

allHonorPairsEx :: Hand
allHonorPairsEx = mkHand
  [ mkEyes c w1
  , mkEyes c w2
  , mkEyes c w3
  , mkEyes c w4
  , mkEyes c d1
  , mkEyes c d2
  ]
  (mkEyes r d3)
  [s3]


-- | 9.0 Seven Pairs

sevenPairsHandEx1 :: Hand
sevenPairsHandEx1 = mkHand
  [ mkEyes c c2
  , mkEyes c c5
  , mkEyes c c8
  , mkEyes c b3
  , mkEyes c w1
  , mkEyes c k7
  ]
  (mkEyes r k8)
  [s3]

sevenPairsHandEx2 :: Hand
sevenPairsHandEx2 = mkHand
  [ mkEyes c c2
  , mkEyes c c2
  , mkEyes c c8
  , mkEyes c b3
  , mkEyes c b5
  , mkEyes c d3
  ]
  (mkEyes r k8)
  [s3]

sevenShiftedPairsHandEx1 :: Hand
sevenShiftedPairsHandEx1 = mkHand
  [ mkEyes c c1
  , mkEyes c c2
  , mkEyes c c3
  , mkEyes c c4
  , mkEyes c c5
  , mkEyes c c6
  ]
  (mkEyes r c7)
  [s3]

sevenShiftedPairsHandEx2 :: Hand
sevenShiftedPairsHandEx2 = mkHand
  [ mkEyes c b3
  , mkEyes c b4
  , mkEyes c b5
  , mkEyes c b6
  , mkEyes c b8
  , mkEyes c b9
  ]
  (mkEyes r b7)
  [f4, s4]

grandChariotEx :: Hand
grandChariotEx = mkHand
  [ mkEyes c c2
  , mkEyes c c3
  , mkEyes c c4
  , mkEyes c c5
  , mkEyes c c6
  , mkEyes c c8
  ]
  (mkEyes r c7)
  [s3]

bambooForestEx :: Hand
bambooForestEx = mkHand
  [ mkEyes c b2
  , mkEyes c b3
  , mkEyes c b4
  , mkEyes c b6
  , mkEyes c b7
  , mkEyes c b8
  ]
  (mkEyes r b5)
  [f3]

numberNeighborhoodEx :: Hand
numberNeighborhoodEx = mkHand
  [ mkEyes c k2
  , mkEyes c k3
  , mkEyes c k4
  , mkEyes c k5
  , mkEyes c k7
  , mkEyes c k8
  ]
  (mkEyes r k6)
  [f1, f3, s3]


-- | 10.0 Color Hands

allGreenEx :: Hand
allGreenEx = mkHand
  [ mkChow r b2
  , mkChow r b2
  , mkPung r b6
  , mkPung c d2
  ]
  (mkEyes c b8)
  [f2, s2]

allRedEx :: Hand
allRedEx = mkHand
  [ mkPung r b1
  , mkPung r b5
  , mkPung c b7
  , mkPung c b9
  ]
  (mkEyes c d1)
  [f1, s1]

allBlueEx :: Hand
allBlueEx = mkHand
  [ mkPung c c8
  , mkPung r w2
  , mkPung r w3
  , mkPung c d3
  ]
  (mkEyes r w3)
  [f3, s3]


-- | 11.0 Irregular Hands

thirteenOrphanEx :: Hand
thirteenOrphanEx = mkSpecial
  [ mkWrap c1, mkWrap c9
  , mkWrap b1, mkWrap b9
  , mkWrap k1, mkWrap k9
  , mkWrap w1, mkWrap w2, mkWrap w3, mkWrap w4
  , mkWrap d1, mkWrap d2, mkWrap d3
  ]
  c1
  [f4]


-- | 12.0 Incidental Bonuses

finalDrawEx :: Hand
finalDrawEx = noHand

finalDiscardEx :: Hand
finalDiscardEx = noHand

winOnKongEx :: Hand
winOnKongEx = noHand

winOnBonusTileEx :: Hand
winOnBonusTileEx = noHand

robbingKongEx :: Hand
robbingKongEx = noHand

blessingOfHeavenEx :: Hand
blessingOfHeavenEx = noHand

blessingOfEarthEx :: Hand
blessingOfEarthEx = noHand


-- | 13.0 Bonus Tiles


bonusTile :: Hand
bonusTile = mkHand
  [ mkChow r c2
  , mkPung r b2
  , mkChow r b8
  , mkPung c w4
  ]
  (mkEyes r d1)
  [f2]

fourFlowersEx :: Hand
fourFlowersEx = mkHand
  [ mkChow r b7
  , mkPung r k4
  , mkChow r k8
  , mkPung c d3
  ]
  (mkEyes r w3)
  [f1, f2, f3, f4]

fourSeasonsEx :: Hand
fourSeasonsEx = mkHand
  [ mkChow r c3
  , mkPung r b4
  , mkChow r k3
  , mkPung c w4
  ]
  (mkEyes r w3)
  [s1, s2, s3, s4]

allBonusTileEx :: Hand
allBonusTileEx = mkHand
  [ mkChow r c7
  , mkPung r b2
  , mkChow r b8
  , mkPung c k4
  ]
  (mkEyes r w3)
  [f1, f2, f3, f4, s1, s2, s3, s4]

