{- SLOW PROGRESS IN CONVERTING THESE OVER TO NEW DATA FORMAT -}
{- MAINLY BECAUSE THIS DEPENDS ON IMPLEMENTATION OF ALL OTHER TO FINALIZE FIRST -}

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

{-
nineTileStraightEx :: Hand
nineTileStraightEx = mkHand

threeConsecutivePungsHand1 :: Hand
threeConsecutivePungsHand1 = mkHand

threeConsecutivePungsHand2 :: Hand
threeConsecutivePungsHand2 = mkHand

fourConsecutivePungsEx :: Hand
fourConsecutivePungsEx = mkHand

threeMothersEx :: Hand
threeMothersEx = mkHand


-- | 6.0 Suit Patterns

mixedOneSuitHand1 :: Hand
mixedOneSuitHand1 = mkHand

mixedOneSuitHand2 :: Hand
mixedOneSuitHand2 = mkHand

pureOneSuitHand1 :: Hand
pureOneSuitHand1 = mkHand

pureOneSuitHand2 :: Hand
pureOneSuitHand2 = mkHand

littleTerminalClubEx :: Hand
littleTerminalClubEx = mkHand

bigTerminalClubEx :: Hand
bigTerminalClubEx = mkHand

nineGatesEx :: Hand
nineGatesEx = mkHand


-- | 7.0 Terminal Tiles

twoTailedTerminalChowsHand1 :: Hand
twoTailedTerminalChowsHand1 = mkHand

twoTailedTerminalChowsHand2 :: Hand
twoTailedTerminalChowsHand2 = mkHand

twoTailedTerminalChowsHand3 :: Hand
twoTailedTerminalChowsHand3 = mkHand

twoTailedTerminalPungsHand1 :: Hand
twoTailedTerminalPungsHand1 = mkHand

twoTailedTerminalPungsHand2 :: Hand
twoTailedTerminalPungsHand2 = mkHand

littleBoundlessMountainHand1 :: Hand
littleBoundlessMountainHand1 = mkHand

littleBoundlessMountainHand2 :: Hand
littleBoundlessMountainHand2 = mkHand

bigBoundlessMountainHand1 :: Hand
bigBoundlessMountainHand1 = mkHand

bigBoundlessMountainHand2 :: Hand
bigBoundlessMountainHand2 = mkHand

mixedLesserTerminalEx :: Hand
mixedLesserTerminalEx = mkHand

pureLesserTerminalEx :: Hand
pureLesserTerminalEx = mkHand

mixedGreaterTerminalHand1 :: Hand
mixedGreaterTerminalHand1 = mkHand

mixedGreaterTerminalHand2 :: Hand
mixedGreaterTerminalHand2 = mkHand

pureGreaterTerminalHand1 :: Hand
pureGreaterTerminalHand1 = mkHand

pureGreaterTermimalHand2 :: Hand
pureGreaterTermimalHand2 = mkHand


-- | 8.0 Honor Tiles

dragonPungEx :: Hand
dragonPungEx = mkHand

seatWindEx :: Hand
seatWindEx = mkHand

littleThreeWindsEx :: Hand
littleThreeWindsEx = mkHand

bigThreeWindsEx :: Hand
bigThreeWindsEx = mkHand

littleFourWindsEx :: Hand
littleFourWindsEx = mkHand

bigFourWindsEx :: Hand
bigFourWindsEx = mkHand

littleThreeDragonsEx :: Hand
littleThreeDragonsEx = mkHand

bigThreeDragonsEx :: Hand
bigThreeDragonsEx = mkHand

allHonorsHand1 :: Hand
allHonorsHand1 = mkHand

allHonorsHand2 :: Hand
allHonorsHand2 = mkHand

allHonorPairsEx :: Hand
allHonorPairsEx = mkHand


-- | 9.0 Seven Pairs

sevenPairsHand1 :: Hand
sevenPairsHand1 = mkHand

sevenPairsHand2 :: Hand
sevenPairsHand2 = mkHand

sevenPairsHand3 :: Hand
sevenPairsHand3 = mkHand

sevenPairsHand4 :: Hand
sevenPairsHand4 = mkHand

sevenShiftedPairsHand1 :: Hand
sevenShiftedPairsHand1 = mkHand

sevenShiftedPairsHand2 :: Hand
sevenShiftedPairsHand2 = mkHand

grandChariotEx :: Hand
grandChariotEx = mkHand

bambooForestEx :: Hand
bambooForestEx = mkHand

numberNeighborhoodEx :: Hand
numberNeighborhoodEx = mkHand


-- | 10.0 Color Hands

allGreenEx :: Hand
allGreenEx = mkHand

allRedEx :: Hand
allRedEx = mkHand

allBlueEx :: Hand
allBlueEx = mkHand


-- | 11.0 Irregular Hands

thirteenOrphanEx :: Hand
thirteenOrphanEx = mkHand
[ ( ['r', 'h', 'B', 'T', 'W', 'D']
                       , [ (C, 1), (C, 9), (B, 1), (B, 9), (K, 1), (K, 9)
                           (W, 1), (W, [(C, 1), (C, 2), (W, 3), (W, 4)
                           (D, 1), (D, [(C, 1), (C, 2), (D, 2), (D, 3)
                         ]
                       )
                     , (['b'], [(F, 1, [(C, 1), ('C')])
                     ]


-- | 12.0 Incidental Bonuses

finalDrawEx :: Hand []
finalDrawEx = mkHand

finalDiscardEx :: Hand []
finalDiscardEx = mkHand

winOnKongEx :: Hand []
winOnKongEx = mkHand

winOnBonusTileEx :: Hand []
winOnBonusTileEx = mkHand

robbingKongEx :: Hand []
robbingKongEx = mkHand

blessingOfHeavenEx :: Hand []
blessingOfHeavenEx = mkHand

blessingOfEarthEx :: Hand []
blessingOfEarthEx = mkHand


-- | 13.0 Bonus Tiles

nonSeatFlowerEx :: Hand
nonSeatFlowerEx = mkHand

nonSeatSeasonEx :: Hand
nonSeatSeasonEx = mkHand

seatFlowerEx :: Hand
seatFlowerEx = mkHand

seatSeasonEx :: Hand
seatSeasonEx = mkHand

fourFlowersEx :: Hand
fourFlowersEx = mkHand

fourSeasonsEx :: Hand
fourSeasonsEx = mkHand

allBonusTileEx :: Hand
allBonusTileEx = mkHand
-}
