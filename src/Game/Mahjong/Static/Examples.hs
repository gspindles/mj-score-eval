-- | Data definition of tiles
--   along with tile related functions
module Game.Mahjong.Static.Examples where

import Game.Mahjong.Class
import Game.Mahjong.Static.Tiles
import Game.Mahjong.Hand
import Game.Mahjong.Meld
import Game.Mahjong.Tile


-------------------------------------------------------------------------------

{- Helpers -}

mkSequence', mkTriplet', mkQuartet', mkPair' :: Status -> Tile -> Maybe Meld
mkSequence' s = mkSequence s . take 3 . iterate next
mkTriplet' s = mkTriplet s . take 3 . repeat
mkQuartet' s = mkQuartet s . take 4 . repeat
mkPair' s = mkPair s . take 2 . repeat


-------------------------------------------------------------------------------

{- Make a list of hands for testing -}

-- | 1.0 Trivial Patterns

chickenEx :: Maybe Hand
chickenEx = mkHand1
  [ mkSequence' Revealed c7
  , mkTriplet' Revealed k2
  , mkSequence' Revealed k7
  , mkTriplet' Concealed b4
  , mkPair' Revealed ww
  ]
  [f1, s2]
  Nothing

allSequencesEx :: Maybe Hand
allSequencesEx = mkHand1
  [ mkSequence' Revealed b4
  , mkSequence' Revealed k7
  , mkSequence' Revealed b2
  , mkSequence' Concealed c1
  , mkPair' Revealed dr
  ]
  [f1, s2]
  Nothing

concealedEx :: Maybe Hand
concealedEx = mkHand1
  [ mkSequence' Concealed c7
  , mkTriplet' Concealed we
  , mkSequence' Concealed k1
  , mkSequence' Concealed b4
  , mkPair' Revealed ww
  ]
  [f1, s2]
  Nothing

selfDrawnEx :: Maybe Hand
selfDrawnEx = mkHand1
  [ mkSequence' Concealed c7
  , mkTriplet' Revealed we
  , mkSequence' Revealed k1
  , mkSequence' Concealed b4
  , mkPair' Concealed ww
  ]
  [f1, s2]
  Nothing

allSimplesEx1 :: Maybe Hand
allSimplesEx1 = mkHand1
  [ mkSequence' Revealed b4
  , mkSequence' Revealed c6
  , mkTriplet' Concealed b3
  , mkTriplet' Revealed k6
  , mkPair' Concealed b2
  ]
  []
  Nothing

allSimplesEx2 :: Maybe Hand
allSimplesEx2 = mkHand1
  [ mkPair' Concealed c2
  , mkPair' Concealed c5
  , mkPair' Concealed c8
  , mkPair' Concealed b3
  , mkPair' Concealed b5
  , mkPair' Concealed k7
  , mkPair' Revealed k8
  ]
  [s3]
  Nothing

allTypesEx1 :: Maybe Hand
allTypesEx1 = mkHand1
  [ mkSequence' Revealed c7
  , mkTriplet' Revealed ws
  , mkQuartet' Promoted k4
  , mkSequence' Concealed b6
  , mkPair' Revealed dw
  ]
  [f1]
  Nothing

allTypesEx2 :: Maybe Hand
allTypesEx2 = mkHand1
  [ mkPair' Concealed c7
  , mkPair' Concealed ws
  , mkPair' Concealed k4
  , mkPair' Concealed b6
  , mkPair' Concealed dr
  , mkPair' Concealed dw
  , mkPair' Revealed ww
  ]
  [f1]
  Nothing

illegalCallEx :: Maybe Hand
illegalCallEx = Nothing



-- | 2.0 Triplets and Quartets

allTripletsEx :: Maybe Hand
allTripletsEx = mkHand1
  [ mkTriplet' Revealed b4
  , mkTriplet' Revealed dg
  , mkQuartet' Concealed k6
  , mkTriplet' Concealed dw
  , mkPair' Revealed b3
  ]
  [f3]
  Nothing

twoConcealedTripletsEx :: Maybe Hand
twoConcealedTripletsEx = mkHand1
  [ mkTriplet' Concealed b4
  , mkTriplet' Concealed c3
  , mkSequence' Revealed k2
  , mkPair' Concealed ws
  , mkSequence' Revealed k6
  ]
  [s2]
  Nothing

threeConcealedTripletsEx :: Maybe Hand
threeConcealedTripletsEx = mkHand1
  [ mkTriplet' Concealed b4
  , mkQuartet' Concealed c3
  , mkSequence' Revealed k2
  , mkPair' Concealed ws
  , mkTriplet' Concealed k6
  ]
  [s2]
  Nothing

fourConcealedTripletsEx1 :: Maybe Hand
fourConcealedTripletsEx1 = mkHand1
  [ mkTriplet' Concealed b4
  , mkTriplet' Concealed c3
  , mkTriplet' Concealed k2
  , mkPair' Concealed ws
  , mkTriplet' Concealed k6
  ]
  [s2]
  Nothing

fourConcealedTripletsEx2 :: Maybe Hand
fourConcealedTripletsEx2 = mkHand1
  [ mkTriplet' Concealed b4
  , mkTriplet' Concealed c4
  , mkQuartet' Concealed k4
  , mkPair' Concealed ws
  , mkTriplet' Concealed k6
  ]
  [s2]
  Nothing

oneQuartetEx :: Maybe Hand
oneQuartetEx = mkHand1
  [ mkQuartet' Revealed c3
  , mkSequence' Concealed b2
  , mkTriplet' Revealed b9
  , mkPair' Concealed dg
  , mkSequence' Revealed k2
  ]
  []
  Nothing

twoQuartetsEx :: Maybe Hand
twoQuartetsEx = mkHand1
  [ mkQuartet' Revealed c3
  , mkSequence' Concealed b2
  , mkTriplet' Revealed b9 >>= flip promoteTriplet b9
  , mkPair' Concealed dg
  , mkSequence' Revealed k2
  ]
  []
  Nothing

threeQuartetsEx :: Maybe Hand
threeQuartetsEx = mkHand1
  [ mkQuartet' Revealed c3
  , mkSequence' Concealed b2
  , mkTriplet' Revealed b9 >>= flip promoteTriplet b9
  , mkPair' Concealed dg
  , mkQuartet' Revealed k5
  ]
  []
  Nothing

fourQuartetsEx :: Maybe Hand
fourQuartetsEx = mkHand1
  [ mkQuartet' Revealed c3
  , mkQuartet' Concealed b6
  , mkTriplet' Revealed b9 >>= flip promoteTriplet b9
  , mkPair' Concealed dg
  , mkTriplet' Revealed k5 >>= flip promoteTriplet k5
  ]
  []
  Nothing



-- | 3.0 Identical Sequences

twoIdenticalSequencesEx :: Maybe Hand
twoIdenticalSequencesEx = mkHand1
  [ mkSequence' Revealed b4
  , mkSequence' Concealed c6
  , mkSequence' Revealed c6
  , mkPair' Concealed k3
  , mkTriplet' Revealed we
  ]
  [s1]
  Nothing


twoIdenticalSequencesTwiceEx :: Maybe Hand
twoIdenticalSequencesTwiceEx = mkHand1
  [ mkSequence' Revealed b4
  , mkSequence' Concealed c6
  , mkSequence' Revealed c6
  , mkPair' Concealed k3
  , mkSequence' Revealed b4
  ]
  [s1]
  Nothing

threeIdenticalSequencesEx :: Maybe Hand
threeIdenticalSequencesEx = mkHand1
  [ mkSequence' Revealed b4
  , mkSequence' Revealed b4
  , mkSequence' Concealed c6
  , mkPair' Concealed k3
  , mkSequence' Revealed b4
  ]
  [s1]
  Nothing

fourIdenticalSequencesEx :: Maybe Hand
fourIdenticalSequencesEx = mkHand1
  [ mkSequence' Revealed b4
  , mkSequence' Revealed b4
  , mkSequence' Concealed b4
  , mkPair' Concealed k3
  , mkSequence' Revealed b4
  ]
  [s1]
  Nothing


-- | 4.0 Similar Sets

threeSimilarSequencesEx :: Maybe Hand
threeSimilarSequencesEx = mkHand1
  [ mkSequence' Revealed k3
  , mkSequence' Revealed b3
  , mkTriplet' Revealed dw
  , mkPair' Concealed b7
  , mkSequence' Revealed c3
  ]
  [s2]
  Nothing


littleThreeSimilarTripletsEx :: Maybe Hand
littleThreeSimilarTripletsEx = mkHand1
  [ mkTriplet' Revealed c6
  , mkQuartet' Revealed k6
  , mkSequence' Concealed c1
  , mkPair' Revealed b6
  , mkTriplet' Revealed dg
  ]
  [s4]
  Nothing

threeSimilarTripletsEx :: Maybe Hand
threeSimilarTripletsEx = mkHand1
  [ mkTriplet' Revealed c6
  , mkQuartet' Promoted k6
  , mkSequence' Concealed c1
  , mkTriplet' Revealed b6 >>= flip promoteTriplet b6
  , mkPair' Revealed dg
  ]
  [s4]
  Nothing



-- | 5.0 Consecutive Sets

threeConsecutiveSequencesEx1 :: Maybe Hand
threeConsecutiveSequencesEx1 = mkHand1
  [ mkSequence' Revealed c1
  , mkSequence' Revealed c2
  , mkSequence' Concealed c3
  , mkTriplet' Concealed b8
  , mkPair' Revealed dg
  ]
  [s1]
  Nothing

threeConsecutiveSequencesEx2 :: Maybe Hand
threeConsecutiveSequencesEx2 = mkHand1
  [ mkSequence' Revealed b1
  , mkSequence' Revealed b3
  , mkSequence' Concealed b5
  , mkTriplet' Concealed k8
  , mkPair' Revealed dg
  ]
  [s1]
  Nothing

nineTileStraightEx :: Maybe Hand
nineTileStraightEx = mkHand1
  [ mkSequence' Revealed b1
  , mkSequence' Revealed b4
  , mkTriplet' Concealed ww
  , mkPair' Concealed k2
  , mkSequence' Revealed b7
  ]
  [f2]
  Nothing

threeConsecutiveSequencesTwiceEx1 :: Maybe Hand
threeConsecutiveSequencesTwiceEx1 = mkHand1
  [ mkSequence' Revealed c2
  , mkSequence' Revealed c3
  , mkSequence' Concealed c4
  , mkSequence' Concealed c6
  , mkPair' Revealed b2
  ]
  [f1, f3, s2]
  Nothing

threeConsecutiveSequencesTwiceEx2 :: Maybe Hand
threeConsecutiveSequencesTwiceEx2 = mkHand1
  [ mkSequence' Revealed c2
  , mkSequence' Revealed c4
  , mkSequence' Concealed c5
  , mkSequence' Concealed c6
  , mkPair' Revealed k7
  ]
  []
  Nothing

fourConsecutiveSequencesEx1 :: Maybe Hand
fourConsecutiveSequencesEx1 = mkHand1
  [ mkSequence' Revealed c1
  , mkSequence' Revealed c2
  , mkSequence' Concealed c3
  , mkSequence' Concealed c4
  , mkPair' Revealed b2
  ]
  [f1]
  Nothing

fourConsecutiveSequencesEx2 :: Maybe Hand
fourConsecutiveSequencesEx2 = mkHand1
  [ mkSequence' Revealed b1
  , mkSequence' Revealed b3
  , mkSequence' Concealed b5
  , mkSequence' Concealed b7
  , mkPair' Revealed k2
  ]
  [f4, s3]
  Nothing


threeConsecutiveTripletsEx :: Maybe Hand
threeConsecutiveTripletsEx = mkHand1
  [ mkTriplet' Revealed k5
  , mkTriplet' Revealed k6
  , mkSequence' Concealed c2
  , mkPair' Concealed dg
  , mkTriplet' Revealed k7
  ]
  []
  Nothing

fourConsecutiveTripletsEx1 :: Maybe Hand
fourConsecutiveTripletsEx1 = mkHand1
  [ mkTriplet' Revealed k5
  , mkTriplet' Revealed k6
  , mkTriplet' Revealed k7
  , mkPair' Concealed c8
  , mkTriplet' Revealed k8
  ]
  [f2]
  Nothing

fourConsecutiveTripletsEx2 :: Maybe Hand
fourConsecutiveTripletsEx2 = mkHand1
  [ mkTriplet' Revealed k5
  , mkTriplet' Revealed k6
  , mkTriplet' Revealed k7
  , mkPair' Concealed dg
  , mkTriplet' Revealed k8
  ]
  []
  Nothing

fourConsecutiveTripletsEx3 :: Maybe Hand
fourConsecutiveTripletsEx3 = mkHand1
  [ mkTriplet' Revealed k5
  , mkTriplet' Revealed k6
  , mkTriplet' Revealed k7
  , mkPair' Concealed k2
  , mkTriplet' Revealed k8
  ]
  []
  Nothing

threeMothersEx :: Maybe Hand
threeMothersEx = mkHand1
  [ mkTriplet' Revealed k5
  , mkTriplet' Revealed k6
  , mkSequence' Revealed k5
  , mkPair' Concealed dg
  , mkTriplet' Revealed k7
  ]
  []
  Nothing



-- | 6.0 Suit Patterns

mixedOneSuitEx1 :: Maybe Hand
mixedOneSuitEx1 = mkHand1
  [ mkTriplet' Revealed k5
  , mkTriplet' Revealed we
  , mkSequence' Revealed k2
  , mkPair' Concealed dg
  , mkSequence' Revealed k7
  ]
  []
  Nothing

mixedOneSuitEx2 :: Maybe Hand
mixedOneSuitEx2 = mkHand1
  [ mkPair' Concealed c2
  , mkPair' Concealed c5
  , mkPair' Concealed c7
  , mkPair' Concealed c8
  , mkPair' Concealed ww
  , mkPair' Concealed wn
  , mkPair' Revealed dr
  ]
  [s3]
  Nothing

pureOneSuitEx1 :: Maybe Hand
pureOneSuitEx1 = mkHand1
  [ mkTriplet' Revealed k1
  , mkSequence' Revealed k3
  , mkSequence' Revealed k4
  , mkPair' Concealed k9
  , mkTriplet' Revealed k8
  ]
  [f3]
  Nothing

pureOneSuitEx2 :: Maybe Hand
pureOneSuitEx2 = mkHand1
  [ mkPair' Concealed c1
  , mkPair' Concealed c2
  , mkPair' Concealed c4
  , mkPair' Concealed c5
  , mkPair' Concealed c7
  , mkPair' Concealed c8
  , mkPair' Revealed c9
  ]
  [s3]
  Nothing


nineGatesEx :: Maybe Hand
nineGatesEx = mkSpecial1
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

twoTailedTerminalSequencesEx1 :: Maybe Hand
twoTailedTerminalSequencesEx1 = mkHand1
  [ mkSequence' Revealed b1
  , mkSequence' Revealed b7
  , mkTriplet' Concealed k3
  , mkPair' Concealed ws
  , mkSequence' Revealed c6
  ]
  [s1]
  Nothing

twoTailedTerminalSequencesEx2 :: Maybe Hand
twoTailedTerminalSequencesEx2 = mkHand1
  [ mkSequence' Revealed b1
  , mkSequence' Revealed b7
  , mkSequence' Concealed k1
  , mkPair' Concealed c3
  , mkSequence' Revealed k7
  ]
  [f2]
  Nothing

twoTailedTerminalSequencesEx3 :: Maybe Hand
twoTailedTerminalSequencesEx3 = mkHand1
  [ mkSequence' Revealed b1
  , mkSequence' Revealed b7
  , mkSequence' Concealed b1
  , mkPair' Concealed c3
  , mkSequence' Revealed b7
  ]
  [f2]
  Nothing

twoTailedTerminalTripletsEx1 :: Maybe Hand
twoTailedTerminalTripletsEx1 = mkHand1
  [ mkTriplet' Revealed b1
  , mkTriplet' Revealed b9
  , mkTriplet' Concealed we
  , mkPair' Concealed c3
  , mkSequence' Revealed k4
  ]
  []
  Nothing

twoTailedTerminalTripletsEx2 :: Maybe Hand
twoTailedTerminalTripletsEx2 = mkHand1
  [ mkTriplet' Revealed b1
  , mkTriplet' Revealed b9
  , mkTriplet' Concealed k1
  , mkPair' Concealed c3
  , mkTriplet' Revealed k9
  ]
  [s3]
  Nothing

twoTailedTerminalsEx1 :: Maybe Hand
twoTailedTerminalsEx1 = mkHand1
  [ mkTriplet' Revealed b1
  , mkTriplet' Revealed b9
  , mkSequence' Concealed b1
  , mkPair' Concealed c3
  , mkSequence' Revealed b7
  ]
  [s1]
  Nothing

twoTailedTerminalsEx2 :: Maybe Hand
twoTailedTerminalsEx2 = mkHand1
  [ mkTriplet' Revealed b1
  , mkTriplet' Revealed b9
  , mkSequence' Concealed b1
  , mkPair' Concealed ww
  , mkSequence' Revealed b7
  ]
  [s1]
  Nothing

twoTailedTerminalsEx3 :: Maybe Hand
twoTailedTerminalsEx3 = mkHand1
  [ mkTriplet' Revealed b1
  , mkTriplet' Revealed b9
  , mkSequence' Concealed b1
  , mkPair' Concealed c9
  , mkSequence' Revealed b7
  ]
  [s1]
  Nothing

twoTailedTerminalsEx4 :: Maybe Hand
twoTailedTerminalsEx4 = mkHand1
  [ mkTriplet' Revealed b1
  , mkTriplet' Revealed b9
  , mkSequence' Concealed b1
  , mkPair' Concealed b5
  , mkSequence' Revealed b7
  ]
  [s1]
  Nothing


mixedLesserTerminalsEx :: Maybe Hand
mixedLesserTerminalsEx = mkHand1
  [ mkSequence' Revealed c1
  , mkTriplet' Revealed c9
  , mkSequence' Concealed b1
  , mkTriplet' Concealed k9
  , mkPair' Revealed ws
  ]
  []
  Nothing

pureLesserTerminalsEx :: Maybe Hand
pureLesserTerminalsEx = mkHand1
  [ mkSequence' Revealed c1
  , mkTriplet' Revealed c9
  , mkSequence' Revealed b1
  , mkTriplet' Concealed k1
  , mkPair' Concealed b9
  ]
  [f3]
  Nothing

mixedGreaterTerminalsEx1 :: Maybe Hand
mixedGreaterTerminalsEx1 = mkHand1
  [ mkTriplet' Revealed c1
  , mkTriplet' Revealed c9
  , mkTriplet' Revealed we
  , mkTriplet' Concealed dr
  , mkPair' Concealed b9
  ]
  [f1, s2]
  Nothing

mixedGreaterTerminalsEx2 :: Maybe Hand
mixedGreaterTerminalsEx2 = mkHand1
  [ mkPair' Concealed c9
  , mkPair' Concealed b9
  , mkPair' Concealed k1
  , mkPair' Concealed ws
  , mkPair' Concealed wn
  , mkPair' Concealed dg
  , mkPair' Revealed we
  ]
  []
  Nothing

pureSuitTerminalsEx1 :: Maybe Hand
pureSuitTerminalsEx1 = mkHand1
  [ mkSequence' Revealed b1
  , mkSequence' Revealed b7
  , mkSequence' Concealed b1
  , mkPair' Concealed b1
  , mkSequence' Revealed b7
  ]
  [f1]
  Nothing

pureSuitTerminalsEx2 :: Maybe Hand
pureSuitTerminalsEx2 = mkHand1
  [ mkSequence' Revealed k1
  , mkSequence' Revealed k1
  , mkSequence' Concealed k1
  , mkPair' Concealed k9
  , mkSequence' Revealed k7
  ]
  [f1]
  Nothing

pureSuitTerminalsEx3 :: Maybe Hand
pureSuitTerminalsEx3 = mkHand1
  [ mkTriplet' Revealed k1
  , mkSequence' Concealed k1
  , mkSequence' Revealed k7
  , mkPair' Concealed k9
  , mkSequence' Revealed k7
  ]
  [s4]
  Nothing

pureSuitTerminalsEx4 :: Maybe Hand
pureSuitTerminalsEx4 = mkHand1
  [ mkSequence' Revealed k1
  , mkSequence' Concealed k1
  , mkSequence' Revealed k7
  , mkPair' Concealed k1
  , mkTriplet' Revealed k9
  ]
  [f4]
  Nothing

pureGreaterTerminalsEx1 :: Maybe Hand
pureGreaterTerminalsEx1 = mkHand1
  [ mkTriplet' Revealed c1
  , mkTriplet' Revealed c9
  , mkTriplet' Revealed b1
  , mkQuartet' Concealed k1
  , mkPair' Concealed b9
  ]
  [s3]
  Nothing

pureGreaterTerminalsEx2 :: Maybe Hand
pureGreaterTerminalsEx2 = mkHand1
  [ mkPair' Concealed c1
  , mkPair' Concealed c9
  , mkPair' Concealed b1
  , mkPair' Concealed b9
  , mkPair' Concealed k1
  , mkPair' Concealed k9
  , mkPair' Revealed k1
  ]
  []
  Nothing



-- | 8.0 Honor Tiles

windTripletEx :: Maybe Hand
windTripletEx = mkHand1
  [ mkTriplet' Revealed c8
  , mkSequence' Revealed b3
  , mkSequence' Revealed k7
  , mkTriplet' Revealed ww
  , mkPair' Concealed ws
  ]
  [s2]
  Nothing

littleThreeWindsEx :: Maybe Hand
littleThreeWindsEx = mkHand1
  [ mkTriplet' Revealed ws
  , mkTriplet' Revealed wn
  , mkSequence' Revealed c3
  , mkQuartet' Concealed k7
  , mkPair' Concealed we
  ]
  [f1, f3]
  Nothing

bigThreeWindsEx :: Maybe Hand
bigThreeWindsEx = mkHand1
  [ mkTriplet' Revealed ws
  , mkTriplet' Revealed wn
  , mkSequence' Revealed c3
  , mkPair' Concealed k7
  , mkTriplet' Concealed we
  ]
  [s1]
  Nothing

littleFourWindsEx :: Maybe Hand
littleFourWindsEx = mkHand1
  [ mkTriplet' Revealed ws
  , mkTriplet' Concealed ww
  , mkTriplet' Revealed wn
  , mkSequence' Revealed c3
  , mkPair' Concealed we
  ]
  [f3, s2]
  Nothing

bigFourWindsEx :: Maybe Hand
bigFourWindsEx = mkHand1
  [ mkTriplet' Revealed ws
  , mkTriplet' Concealed ww
  , mkTriplet' Revealed wn
  , mkPair' Concealed k7
  , mkTriplet' Revealed we
  ]
  [f4, s2]
  Nothing


dragonTripletEx1 :: Maybe Hand
dragonTripletEx1 = mkHand1
  [ mkSequence' Revealed c1
  , mkSequence' Revealed k2
  , mkTriplet' Revealed dr
  , mkPair' Concealed dw
  , mkTriplet' Revealed ww
  ]
  []
  Nothing

dragonTripletEx2 :: Maybe Hand
dragonTripletEx2 = mkHand1
  [ mkSequence' Revealed c1
  , mkSequence' Revealed k2
  , mkTriplet' Revealed dr
  , mkPair' Concealed ww
  , mkTriplet' Revealed dg
  ]
  []
  Nothing

littleThreeDragonsEx :: Maybe Hand
littleThreeDragonsEx = mkHand1
  [ mkTriplet' Revealed dr
  , mkTriplet' Revealed dg
  , mkSequence' Concealed b3
  , mkTriplet' Concealed k8
  , mkPair' Revealed dw
  ]
  [s3]
  Nothing

bigThreeDragonsEx :: Maybe Hand
bigThreeDragonsEx = mkHand1
  [ mkTriplet' Revealed dr
  , mkTriplet' Revealed dg
  , mkSequence' Concealed b3
  , mkPair' Concealed k8
  , mkTriplet' Revealed dw
  ]
  [f2]
  Nothing


allHonorsEx1 :: Maybe Hand
allHonorsEx1 = mkHand1
  [ mkTriplet' Revealed we
  , mkTriplet' Revealed wn
  , mkTriplet' Revealed dr
  , mkTriplet' Concealed dg
  , mkTriplet' Revealed ww
  ]
  [s1]
  Nothing

allHonorsEx2 :: Maybe Hand
allHonorsEx2 = mkHand1
  [ mkPair' Concealed we
  , mkPair' Concealed ww
  , mkPair' Concealed ww
  , mkPair' Concealed wn
  , mkPair' Concealed dr
  , mkPair' Concealed dg
  , mkPair' Revealed we
  ]
  [s3]
  Nothing

allHonorPairsEx :: Maybe Hand
allHonorPairsEx = mkHand1
  [ mkPair' Concealed we
  , mkPair' Concealed ws
  , mkPair' Concealed ww
  , mkPair' Concealed wn
  , mkPair' Concealed dr
  , mkPair' Concealed dg
  , mkPair' Revealed dw
  ]
  [s3]
  Nothing



-- | 9.0 Color Hands

allGreenEx1 :: Maybe Hand
allGreenEx1 = mkHand1
  [ mkSequence' Revealed b2
  , mkSequence' Revealed b2
  , mkTriplet' Revealed b6
  , mkTriplet' Concealed dg
  , mkPair' Concealed b8
  ]
  [f2, s2]
  Nothing

allGreenEx2 :: Maybe Hand
allGreenEx2 = mkHand1
  [ mkPair' Concealed b2
  , mkPair' Concealed b3
  , mkPair' Concealed b4
  , mkPair' Concealed b6
  , mkPair' Concealed b8
  , mkPair' Concealed dg
  , mkPair' Revealed dg
  ]
  [f3]
  Nothing

allRedEx :: Maybe Hand
allRedEx = mkHand1
  [ mkTriplet' Revealed b1
  , mkTriplet' Revealed b5
  , mkTriplet' Concealed b7
  , mkTriplet' Concealed b9
  , mkPair' Concealed dr
  ]
  [f1, s1]
  Nothing

allBlueEx :: Maybe Hand
allBlueEx = mkHand1
  [ mkTriplet' Concealed c8
  , mkTriplet' Revealed ws
  , mkTriplet' Revealed ww
  , mkTriplet' Concealed dw
  , mkPair' Revealed ww
  ]
  [f3, s3]
  Nothing



-- | 10.0 Irregular Hands

thirteenOrphansImpureEx :: Maybe Hand
thirteenOrphansImpureEx = mkSpecial1
  [ c1, c1
  , b1, b9
  , k1, k9
  , we, ws, ww, wn
  , dr, dg, dw
  ]
  c9
  [f4]
  Nothing

thirteenOrphansPureEx :: Maybe Hand
thirteenOrphansPureEx = mkSpecial1
  [ c1, c9
  , b1, b9
  , k1, k9
  , we, ws, ww, wn
  , dr, dg, dw
  ]
  c1
  [f4]
  Nothing

sevenPairsEx1 :: Maybe Hand
sevenPairsEx1 = mkHand1
  [ mkPair' Concealed c2
  , mkPair' Concealed c5
  , mkPair' Concealed c8
  , mkPair' Concealed b3
  , mkPair' Concealed we
  , mkPair' Concealed k7
  , mkPair' Revealed k8
  ]
  [s3]
  Nothing

sevenPairsEx2 :: Maybe Hand
sevenPairsEx2 = mkHand1
  [ mkPair' Concealed c2
  , mkPair' Concealed c2
  , mkPair' Concealed c8
  , mkPair' Concealed b3
  , mkPair' Concealed b5
  , mkPair' Concealed dw
  , mkPair' Revealed k8
  ]
  [s3]
  Nothing

sevenConsecutivePairsEx1 :: Maybe Hand
sevenConsecutivePairsEx1 = mkHand1
  [ mkPair' Concealed c1
  , mkPair' Concealed c2
  , mkPair' Concealed c3
  , mkPair' Concealed c4
  , mkPair' Concealed c5
  , mkPair' Concealed c6
  , mkPair' Revealed c7
  ]
  [s3]
  Nothing

sevenConsecutivePairsEx2 :: Maybe Hand
sevenConsecutivePairsEx2 = mkHand1
  [ mkPair' Concealed b3
  , mkPair' Concealed b4
  , mkPair' Concealed b5
  , mkPair' Concealed b6
  , mkPair' Concealed b8
  , mkPair' Concealed b9
  , mkPair' Revealed b7
  ]
  [f4, s4]
  Nothing

grandChariotEx :: Maybe Hand
grandChariotEx = mkHand1
  [ mkPair' Concealed c2
  , mkPair' Concealed c3
  , mkPair' Concealed c4
  , mkPair' Concealed c5
  , mkPair' Concealed c6
  , mkPair' Concealed c8
  , mkPair' Revealed c7
  ]
  [s3]
  Nothing

bambooForestEx :: Maybe Hand
bambooForestEx = mkHand1
  [ mkPair' Concealed b2
  , mkPair' Concealed b3
  , mkPair' Concealed b4
  , mkPair' Concealed b6
  , mkPair' Concealed b7
  , mkPair' Concealed b8
  , mkPair' Revealed b5
  ]
  [f3]
  Nothing

numerousNeighborsEx :: Maybe Hand
numerousNeighborsEx = mkHand1
  [ mkPair' Concealed k2
  , mkPair' Concealed k3
  , mkPair' Concealed k4
  , mkPair' Concealed k5
  , mkPair' Concealed k7
  , mkPair' Concealed k8
  , mkPair' Revealed k6
  ]
  [f1, f3, s3]
  Nothing



-- | 11.0 Incidental Bonuses

finalDrawEx :: Maybe Hand
finalDrawEx        = (flip addHandInfo OnSeabed)            <$> chickenEx

finalDiscardEx :: Maybe Hand
finalDiscardEx     = (flip addHandInfo OnRiverbed)          <$> chickenEx

winOnQuartetEx :: Maybe Hand
winOnQuartetEx     = (flip addHandInfo OnQuartetSupplement) <$> chickenEx

winOnBonusTileEx :: Maybe Hand
winOnBonusTileEx   = (flip addHandInfo OnBonusSupplement)   <$> chickenEx

robbingAQuartetEx :: Maybe Hand
robbingAQuartetEx  = (flip addHandInfo OnQuartetRobbing)    <$> chickenEx

blessingOfHeavenEx :: Maybe Hand
blessingOfHeavenEx = (flip addHandInfo OnFirstDraw)         <$> chickenEx

blessingOfEarthEx :: Maybe Hand
blessingOfEarthEx  = (flip addHandInfo OnFirstDiscard)      <$> chickenEx



-- | 12.0 Bonus Tiles

bonusTilesEx :: Maybe Hand
bonusTilesEx = mkHand1
  [ mkSequence' Revealed c2
  , mkTriplet' Revealed b2
  , mkSequence' Revealed b7
  , mkTriplet' Concealed wn
  , mkPair' Revealed dr
  ]
  [f2]
  Nothing

fourFlowersEx :: Maybe Hand
fourFlowersEx = mkHand1
  [ mkSequence' Revealed b7
  , mkTriplet' Revealed k4
  , mkSequence' Revealed k7
  , mkTriplet' Concealed dw
  , mkPair' Revealed ww
  ]
  [f1, f2, f3, f4]
  Nothing

fourSeasonsEx :: Maybe Hand
fourSeasonsEx = mkHand1
  [ mkSequence' Revealed c3
  , mkTriplet' Revealed b4
  , mkSequence' Revealed k3
  , mkTriplet' Concealed wn
  , mkPair' Revealed ww
  ]
  [s1, s2, s3, s4]
  Nothing

allBonusTilesEx :: Maybe Hand
allBonusTilesEx = mkHand1
  [ mkSequence' Revealed c7
  , mkTriplet' Revealed b2
  , mkSequence' Revealed b7
  , mkTriplet' Concealed k4
  , mkPair' Revealed ww
  ]
  [f1, f2, f3, f4, s1, s2, s3, s4]
  Nothing

