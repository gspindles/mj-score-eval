-- |
-- Module      :  Game.Mahjong.Examples
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definition of tiles
--   along with tile related functions
module Game.Mahjong.Examples where

import Game.Mahjong.Class (pp)
import Game.Mahjong.Hand
import Game.Mahjong.Meld
import Game.Mahjong.Tile


-------------------------------------------------------------------------------

{- Aliases -}

r, c :: Status
r = Revealed
c = Concealed


-------------------------------------------------------------------------------

{- Make a list of hands for testing -}

-- | 1.0 Trivial Patterns

chickenEx :: Maybe Hand
chickenEx = mkHand
  [ mkChow r c7
  , mkPung r k2
  , mkChow r k7
  , mkPung c b4
  , mkEyes r ww
  ]
  [f1, s2]
  Nothing

allChowsEx :: Maybe Hand
allChowsEx = mkHand
  [ mkChow r b4
  , mkChow r c7
  , mkChow r b2
  , mkChow c c1
  , mkEyes c dr
  ]
  [f1, s2]
  Nothing

concealedEx :: Maybe Hand
concealedEx = mkHand
  [ mkChow c c7
  , mkPung c we
  , mkChow c k1
  , mkChow c b4
  , mkEyes r ww
  ]
  [f1, s2]
  Nothing

selfDrawnEx :: Maybe Hand
selfDrawnEx = mkHand
  [ mkChow c c7
  , mkPung r we
  , mkChow r k1
  , mkChow c b4
  , mkEyes c ww
  ]
  [f1, s2]
  Nothing

allSimplesEx1 :: Maybe Hand
allSimplesEx1 = mkHand
  [ mkChow r b4
  , mkChow r c6
  , mkPung c b3
  , mkPung r k6
  , mkEyes c b2
  ]
  []
  Nothing

allSimplesEx2 :: Maybe Hand
allSimplesEx2 = mkHand
  [ mkEyes c c2
  , mkEyes c c5
  , mkEyes c c8
  , mkEyes c b3
  , mkEyes c b5
  , mkEyes c k7
  , mkEyes r k8
  ]
  [s3]
  Nothing

allTypesEx :: Maybe Hand
allTypesEx = mkHand
  [ mkChow r c7
  , mkPung r ws
  , mkPung c k4
  , mkChow c b6
  , mkEyes r dw
  ]
  [f1]
  Nothing



-- | 2.0 Identical Chows

twoIdenticalChowsEx :: Maybe Hand
twoIdenticalChowsEx = mkHand
  [ mkChow r b4
  , mkChow c c6
  , mkChow r c6
  , mkEyes c k3
  , mkPung r we
  ]
  [s1]
  Nothing


twoIdenticalChowsTwiceEx :: Maybe Hand
twoIdenticalChowsTwiceEx = mkHand
  [ mkChow r b4
  , mkChow c c6
  , mkChow r c6
  , mkEyes c k3
  , mkChow r b4
  ]
  [s1]
  Nothing

threeIdenticalChowsEx :: Maybe Hand
threeIdenticalChowsEx = mkHand
  [ mkChow r b4
  , mkChow r b4
  , mkChow c c6
  , mkEyes c k3
  , mkChow r b4
  ]
  [s1]
  Nothing

fourIdenticalChowsEx :: Maybe Hand
fourIdenticalChowsEx = mkHand
  [ mkChow r b4
  , mkChow r b4
  , mkChow c b4
  , mkEyes c k3
  , mkChow r b4
  ]
  [s1]
  Nothing



-- | 3.0 Pungs and Kongs

allPungsEx :: Maybe Hand
allPungsEx = mkHand
  [ mkPung r b4
  , mkPung c dg
  , mkKong c k6
  , mkPung c dg
  , mkEyes r b3
  ]
  [f3]
  Nothing

twoConcealedPungsEx :: Maybe Hand
twoConcealedPungsEx = mkHand
  [ mkPung c b4
  , mkPung c c3
  , mkChow r k2
  , mkEyes c ws
  , mkChow r k6
  ]
  [s2]
  Nothing

threeConcealedPungsEx :: Maybe Hand
threeConcealedPungsEx = mkHand
  [ mkPung c b4
  , mkKong c c3
  , mkChow r k2
  , mkEyes c ws
  , mkPung c k6
  ]
  [s2]
  Nothing

fourConcealedPungsEx :: Maybe Hand
fourConcealedPungsEx = mkHand
  [ mkPung c b4
  , mkPung c c3
  , mkPung c k2
  , mkEyes c ws
  , mkPung c k6
  ]
  [s2]
  Nothing


oneKongEx :: Maybe Hand
oneKongEx = mkHand
  [ mkKong r c3
  , mkChow c b2
  , mkPung r b9
  , mkEyes c dg
  , mkChow r k2
  ]
  []
  Nothing

twoKongsEx :: Maybe Hand
twoKongsEx = mkHand
  [ mkKong r c3
  , mkChow c b2
  , mkKong r b9
  , mkEyes c dg
  , mkChow r k2
  ]
  []
  Nothing

threeKongsEx :: Maybe Hand
threeKongsEx = mkHand
  [ mkKong r c3
  , mkChow c b2
  , mkKong r b9
  , mkEyes c dg
  , mkKong r k2
  ]
  []
  Nothing

fourKongsEx :: Maybe Hand
fourKongsEx = mkHand
  [ mkKong r c3
  , mkKong c b2
  , mkKong r b9
  , mkEyes c dg
  , mkKong r k2
  ]
  []
  Nothing



-- | 4.0 Similar Sets

threeSimilarChowsEx :: Maybe Hand
threeSimilarChowsEx = mkHand
  [ mkChow r k3
  , mkChow r b3
  , mkPung r dw
  , mkEyes c b7
  , mkChow r c3
  ]
  [s2]
  Nothing


littleThreeSimilarPungsEx :: Maybe Hand
littleThreeSimilarPungsEx = mkHand
  [ mkPung r c6
  , mkKong r k6
  , mkChow c c1
  , mkEyes r c6
  , mkPung r dg
  ]
  [s4]
  Nothing

threeSimilarPungsEx :: Maybe Hand
threeSimilarPungsEx = mkHand
  [ mkPung r c6
  , mkKong r k6
  , mkChow c c1
  , mkPung r c6
  , mkEyes r dg
  ]
  [s4]
  Nothing



-- | 5.0 Consecutive Sets

threeConsecutiveChowsEx1 :: Maybe Hand
threeConsecutiveChowsEx1 = mkHand
  [ mkChow r c1
  , mkChow r c2
  , mkChow c c3
  , mkPung c b8
  , mkEyes r dg
  ]
  [s1]
  Nothing

threeConsecutiveChowsEx2 :: Maybe Hand
threeConsecutiveChowsEx2 = mkHand
  [ mkChow r c1
  , mkChow r c3
  , mkChow c c5
  , mkPung c b8
  , mkEyes r dg
  ]
  [s1]
  Nothing

nineTileStraightEx :: Maybe Hand
nineTileStraightEx = mkHand
  [ mkChow r b1
  , mkChow r b4
  , mkPung c ww
  , mkEyes c k2
  , mkChow r b7
  ]
  [f2]
  Nothing

threeConsecutiveChowsTwiceEx1 :: Maybe Hand
threeConsecutiveChowsTwiceEx1 = mkHand
  [ mkChow r c2
  , mkChow r c3
  , mkChow c c4
  , mkChow c c6
  , mkEyes r b2
  ]
  [f1, f3, s2]
  Nothing

threeConsecutiveChowsTwiceEx2 :: Maybe Hand
threeConsecutiveChowsTwiceEx2 = mkHand
  [ mkChow r c2
  , mkChow r c4
  , mkChow c c5
  , mkChow c c6
  , mkEyes r k7
  ]
  []
  Nothing

fourConsecutiveChowsEx1 :: Maybe Hand
fourConsecutiveChowsEx1 = mkHand
  [ mkChow r c1
  , mkChow r c2
  , mkChow c c3
  , mkChow c c4
  , mkEyes r b2
  ]
  [f1]
  Nothing

fourConsecutiveChowsEx2 :: Maybe Hand
fourConsecutiveChowsEx2 = mkHand
  [ mkChow r c1
  , mkChow r c3
  , mkChow c c5
  , mkChow c b7
  , mkEyes r k2
  ]
  [f4, s3]
  Nothing


threeConsecutivePungsEx :: Maybe Hand
threeConsecutivePungsEx = mkHand
  [ mkPung r k5
  , mkPung r k6
  , mkChow c c2
  , mkEyes c dg
  , mkPung r k7
  ]
  []
  Nothing

fourConsecutivePungsEx :: Maybe Hand
fourConsecutivePungsEx = mkHand
  [ mkPung r k5
  , mkPung r k6
  , mkChow r k7
  , mkEyes c dg
  , mkPung r k8
  ]
  []
  Nothing

threeMothersEx :: Maybe Hand
threeMothersEx = mkHand
  [ mkPung r k5
  , mkPung r k6
  , mkChow r k5
  , mkEyes c dg
  , mkPung r k7
  ]
  []
  Nothing



-- | 6.0 Suit Patterns

mixedOneSuitEx1 :: Maybe Hand
mixedOneSuitEx1 = mkHand
  [ mkPung r k5
  , mkPung r we
  , mkChow r k2
  , mkEyes c dg
  , mkChow r k7
  ]
  []
  Nothing

mixedOneSuitEx2 :: Maybe Hand
mixedOneSuitEx2 = mkHand
  [ mkEyes c c2
  , mkEyes c c5
  , mkEyes c c7
  , mkEyes c c8
  , mkEyes c ww
  , mkEyes c wn
  , mkEyes r dr
  ]
  [s3]
  Nothing

pureOneSuitEx1 :: Maybe Hand
pureOneSuitEx1 = mkHand
  [ mkPung r k1
  , mkChow r k3
  , mkChow r k4
  , mkEyes c k9
  , mkPung r k8
  ]
  [f3]
  Nothing

pureOneSuitEx2 :: Maybe Hand
pureOneSuitEx2 = mkHand
  [ mkEyes c c1
  , mkEyes c c2
  , mkEyes c c4
  , mkEyes c c5
  , mkEyes c c7
  , mkEyes c c8
  , mkEyes r c9
  ]
  [s3]
  Nothing


nineGatesEx :: Maybe Hand
nineGatesEx = mkSpecial
  [ b1, b1, b1
  , b2, b3, b4
  , b5
  , b6, b7, b8
  , b9, b9, b9
  ]
  b5
  [f3]
  Nothing



-- | 7.0 Terminal Tiles

twoTailedTerminalChowsEx1 :: Maybe Hand
twoTailedTerminalChowsEx1 = mkHand
  [ mkChow r b1
  , mkChow r b7
  , mkPung c k3
  , mkEyes c ws
  , mkChow r c6
  ]
  [s1]
  Nothing

twoTailedTerminalChowsEx2 :: Maybe Hand
twoTailedTerminalChowsEx2 = mkHand
  [ mkChow r b1
  , mkChow r b7
  , mkChow c k1
  , mkEyes c c3
  , mkChow r k7
  ]
  [f2]
  Nothing

twoTailedTerminalPungsEx1 :: Maybe Hand
twoTailedTerminalPungsEx1 = mkHand
  [ mkPung r b1
  , mkPung r b9
  , mkPung c we
  , mkEyes c c3
  , mkChow r k4
  ]
  []
  Nothing

twoTailedTerminalPungsEx2 :: Maybe Hand
twoTailedTerminalPungsEx2 = mkHand
  [ mkPung r b1
  , mkPung r b9
  , mkPung c k1
  , mkEyes c c3
  , mkPung r k9
  ]
  [s3]
  Nothing

twoTailedTerminalsEx1 :: Maybe Hand
twoTailedTerminalsEx1 = mkHand
  [ mkPung r b1
  , mkPung r b9
  , mkChow c b1
  , mkEyes c c3
  , mkChow r b7
  ]
  [s1]
  Nothing

twoTailedTerminalsEx2 :: Maybe Hand
twoTailedTerminalsEx2 = mkHand
  [ mkPung r b1
  , mkPung r b9
  , mkChow c b1
  , mkEyes c b5
  , mkChow r b7
  ]
  [s1]
  Nothing

littleBoundlessMountainHandx1 :: Maybe Hand
littleBoundlessMountainHandx1 = mkHand
  [ mkPung r b1
  , mkPung r b9
  , mkChow c b1
  , mkEyes c b5
  , mkChow r b7
  ]
  [f1]
  Nothing

littleBoundlessMountainEx2 :: Maybe Hand
littleBoundlessMountainEx2 = mkHand
  [ mkChow r k1
  , mkChow r k1
  , mkChow c k1
  , mkEyes c k9
  , mkChow r k7
  ]
  [f1]
  Nothing

bigBoundlessMountainEx1 :: Maybe Hand
bigBoundlessMountainEx1 = mkHand
  [ mkPung r k1
  , mkChow c k1
  , mkChow r k7
  , mkEyes c k9
  , mkChow r k7
  ]
  [s4]
  Nothing

bigBoundlessMountainEx2 :: Maybe Hand
bigBoundlessMountainEx2 = mkHand
  [ mkChow r k1
  , mkChow c k1
  , mkChow r k7
  , mkEyes c k1
  , mkPung r k9
  ]
  [f4]
  Nothing


mixedLesserTerminalEx :: Maybe Hand
mixedLesserTerminalEx = mkHand
  [ mkChow r c1
  , mkPung r c9
  , mkChow c b1
  , mkPung c k9
  , mkEyes r ws
  ]
  []
  Nothing

pureLesserTerminalEx :: Maybe Hand
pureLesserTerminalEx = mkHand
  [ mkChow r c1
  , mkPung r c9
  , mkChow r b1
  , mkPung c k1
  , mkEyes c b9
  ]
  [f3]
  Nothing

mixedGreaterTerminalEx1 :: Maybe Hand
mixedGreaterTerminalEx1 = mkHand
  [ mkPung r c1
  , mkPung r c9
  , mkPung r we
  , mkPung c dr
  , mkEyes c b9
  ]
  [f1, s2]
  Nothing

mixedGreaterTerminalEx2 :: Maybe Hand
mixedGreaterTerminalEx2 = mkHand
  [ mkEyes c c9
  , mkEyes c b9
  , mkEyes c k1
  , mkEyes c ws
  , mkEyes c wn
  , mkEyes c dg
  , mkEyes r we
  ]
  []
  Nothing

pureGreaterTerminalEx1 :: Maybe Hand
pureGreaterTerminalEx1 = mkHand
  [ mkPung r c1
  , mkPung r c9
  , mkPung r b1
  , mkKong c k1
  , mkEyes c b9
  ]
  [s3]
  Nothing

pureGreaterTerminalEx2 :: Maybe Hand
pureGreaterTerminalEx2 = mkHand
  [ mkEyes c c1
  , mkEyes c c9
  , mkEyes c b1
  , mkEyes c b9
  , mkEyes c k1
  , mkEyes c k9
  , mkEyes r k1
  ]
  []
  Nothing



-- | 8.0 Honor Tiles

windPungEx :: Maybe Hand
windPungEx = mkHand
  [ mkPung r c8
  , mkChow r b3
  , mkChow r k7
  , mkPung r ww
  , mkEyes c ws
  ]
  [s2]
  Nothing

littleThreeWindsEx :: Maybe Hand
littleThreeWindsEx = mkHand
  [ mkPung r ws
  , mkPung r wn
  , mkChow r c3
  , mkKong c k7
  , mkEyes c we
  ]
  [f1, f3]
  Nothing

bigThreeWindsEx :: Maybe Hand
bigThreeWindsEx = mkHand
  [ mkPung r ws
  , mkPung r wn
  , mkChow r c3
  , mkEyes c k7
  , mkPung c we
  ]
  [s1]
  Nothing

littleFourWindsEx :: Maybe Hand
littleFourWindsEx = mkHand
  [ mkPung r ws
  , mkPung c ww
  , mkPung r wn
  , mkChow r c3
  , mkEyes c we
  ]
  [f3, s2]
  Nothing

bigFourWindsEx :: Maybe Hand
bigFourWindsEx = mkHand
  [ mkPung r ws
  , mkPung c ww
  , mkPung r wn
  , mkEyes c k7
  , mkPung r we
  ]
  [f4, s2]
  Nothing


dragonPungEx :: Maybe Hand
dragonPungEx = mkHand
  [ mkChow r c1
  , mkChow r k2
  , mkPung r dr
  , mkEyes c dw
  , mkPung r ww
  ]
  []
  Nothing

littleThreeDragonsEx :: Maybe Hand
littleThreeDragonsEx = mkHand
  [ mkPung r dr
  , mkPung r dg
  , mkChow c b3
  , mkPung c k8
  , mkEyes r dw
  ]
  [s3]
  Nothing

bigThreeDragonsEx :: Maybe Hand
bigThreeDragonsEx = mkHand
  [ mkPung r dr
  , mkPung r dg
  , mkChow c b3
  , mkEyes c k8
  , mkPung r dw
  ]
  [f2]
  Nothing


allHonorsEx1 :: Maybe Hand
allHonorsEx1 = mkHand
  [ mkPung r we
  , mkPung r wn
  , mkPung r dr
  , mkPung c dg
  , mkPung r ww
  ]
  [s1]
  Nothing

allHonorsEx2 :: Maybe Hand
allHonorsEx2 = mkHand
  [ mkEyes c we
  , mkEyes c ww
  , mkEyes c ww
  , mkEyes c wn
  , mkEyes c dr
  , mkEyes c dg
  , mkEyes r we
  ]
  [s3]
  Nothing

allHonorPairsEx :: Maybe Hand
allHonorPairsEx = mkHand
  [ mkEyes c we
  , mkEyes c ws
  , mkEyes c ww
  , mkEyes c wn
  , mkEyes c dr
  , mkEyes c dg
  , mkEyes r dw
  ]
  [s3]
  Nothing



-- | 9.0 Seven Pairs

sevenPairsEx1 :: Maybe Hand
sevenPairsEx1 = mkHand
  [ mkEyes c c2
  , mkEyes c c5
  , mkEyes c c8
  , mkEyes c b3
  , mkEyes c we
  , mkEyes c k7
  , mkEyes r k8
  ]
  [s3]
  Nothing

sevenPairsEx2 :: Maybe Hand
sevenPairsEx2 = mkHand
  [ mkEyes c c2
  , mkEyes c c2
  , mkEyes c c8
  , mkEyes c b3
  , mkEyes c b5
  , mkEyes c dw
  , mkEyes r k8
  ]
  [s3]
  Nothing

sevenShiftedPairsEx1 :: Maybe Hand
sevenShiftedPairsEx1 = mkHand
  [ mkEyes c c1
  , mkEyes c c2
  , mkEyes c c3
  , mkEyes c c4
  , mkEyes c c5
  , mkEyes c c6
  , mkEyes r c7
  ]
  [s3]
  Nothing

sevenShiftedPairsEx2 :: Maybe Hand
sevenShiftedPairsEx2 = mkHand
  [ mkEyes c b3
  , mkEyes c b4
  , mkEyes c b5
  , mkEyes c b6
  , mkEyes c b8
  , mkEyes c b9
  , mkEyes r b7
  ]
  [f4, s4]
  Nothing

grandChariotEx :: Maybe Hand
grandChariotEx = mkHand
  [ mkEyes c c2
  , mkEyes c c3
  , mkEyes c c4
  , mkEyes c c5
  , mkEyes c c6
  , mkEyes c c8
  , mkEyes r c7
  ]
  [s3]
  Nothing

bambooForestEx :: Maybe Hand
bambooForestEx = mkHand
  [ mkEyes c b2
  , mkEyes c b3
  , mkEyes c b4
  , mkEyes c b6
  , mkEyes c b7
  , mkEyes c b8
  , mkEyes r b5
  ]
  [f3]
  Nothing

numberNeighborhoodEx :: Maybe Hand
numberNeighborhoodEx = mkHand
  [ mkEyes c k2
  , mkEyes c k3
  , mkEyes c k4
  , mkEyes c k5
  , mkEyes c k7
  , mkEyes c k8
  , mkEyes r k6
  ]
  [f1, f3, s3]
  Nothing



-- | 10.0 Color Hands

allGreenEx :: Maybe Hand
allGreenEx = mkHand
  [ mkChow r b2
  , mkChow r b2
  , mkPung r b6
  , mkPung c dg
  , mkEyes c b8
  ]
  [f2, s2]
  Nothing

allRedEx :: Maybe Hand
allRedEx = mkHand
  [ mkPung r b1
  , mkPung r b5
  , mkPung c b7
  , mkPung c b9
  , mkEyes c dr
  ]
  [f1, s1]
  Nothing

allBlueEx :: Maybe Hand
allBlueEx = mkHand
  [ mkPung c c8
  , mkPung r ws
  , mkPung r ww
  , mkPung c dw
  , mkEyes r ww
  ]
  [f3, s3]
  Nothing



-- | 11.0 Irregular Hands

thirteenOrphanImpureEx :: Maybe Hand
thirteenOrphanImpureEx = mkSpecial
  [ c1, c1
  , b1, b9
  , k1, k9
  , we, ws, ww, wn
  , dr, dg, dw
  ]
  c9
  [f4]
  Nothing

thirteenOrphanPureEx :: Maybe Hand
thirteenOrphanPureEx = mkSpecial
  [ c1, c9
  , b1, b9
  , k1, k9
  , we, ws, ww, wn
  , dr, dg, dw
  ]
  c1
  [f4]
  Nothing



-- | 12.0 Incidental Bonuses
finalDrawEx :: Maybe Hand
finalDrawEx        = fmap (flip addHandInfo OnSeabed) chickenEx

finalDiscardEx :: Maybe Hand
finalDiscardEx     = fmap (flip addHandInfo OnRiverbed) chickenEx

winOnKongEx :: Maybe Hand
winOnKongEx        = fmap (flip addHandInfo OnKongSupplement) chickenEx

winOnBonusTileEx :: Maybe Hand
winOnBonusTileEx   = fmap (flip addHandInfo OnBonusSupplement) chickenEx

robbingKongEx :: Maybe Hand
robbingKongEx      = fmap (flip addHandInfo OnKongRobbing) chickenEx

blessingOfHeavenEx :: Maybe Hand
blessingOfHeavenEx = fmap (flip addHandInfo OnFirstDraw) chickenEx

blessingOfEarthEx :: Maybe Hand
blessingOfEarthEx  = fmap (flip addHandInfo OnFirstDiscard) chickenEx



-- | 13.0 Bonus Tiles

bonusTile :: Maybe Hand
bonusTile = mkHand
  [ mkChow r c2
  , mkPung r b2
  , mkChow r b7
  , mkPung c wn
  , mkEyes r dr
  ]
  [f2]
  Nothing

fourFlowersEx :: Maybe Hand
fourFlowersEx = mkHand
  [ mkChow r b7
  , mkPung r k4
  , mkChow r k7
  , mkPung c dw
  , mkEyes r ww
  ]
  [f1, f2, f3, f4]
  Nothing

fourSeasonsEx :: Maybe Hand
fourSeasonsEx = mkHand
  [ mkChow r c3
  , mkPung r b4
  , mkChow r k3
  , mkPung c wn
  , mkEyes r ww
  ]
  [s1, s2, s3, s4]
  Nothing

allBonusTileEx :: Maybe Hand
allBonusTileEx = mkHand
  [ mkChow r c7
  , mkPung r b2
  , mkChow r b7
  , mkPung c k4
  , mkEyes r ww
  ]
  [f1, f2, f3, f4, s1, s2, s3, s4]
  Nothing

