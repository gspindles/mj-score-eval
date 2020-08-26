-- | Data definition of tiles
--   along with tile related functions
module Game.Mahjong.Static.Examples where

import Game.Mahjong.Class
import Game.Mahjong.Static.Tiles
import Game.Mahjong.Hand
import Game.Mahjong.Meld
import Game.Mahjong.Tile


-------------------------------------------------------------------------------

{- Aliases -}

r, c, p :: Status
r = Revealed
c = Concealed
p = Promoted

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
  [ mkSequence' r c7
  , mkTriplet' r k2
  , mkSequence' r k7
  , mkTriplet' c b4
  , mkPair' r ww
  ]
  [f1, s2]
  Nothing

allSequencesEx :: Maybe Hand
allSequencesEx = mkHand1
  [ mkSequence' r b4
  , mkSequence' r k7
  , mkSequence' r b2
  , mkSequence' c c1
  , mkPair' r dr
  ]
  [f1, s2]
  Nothing

concealedEx :: Maybe Hand
concealedEx = mkHand1
  [ mkSequence' c c7
  , mkTriplet' c we
  , mkSequence' c k1
  , mkSequence' c b4
  , mkPair' r ww
  ]
  [f1, s2]
  Nothing

selfDrawnEx :: Maybe Hand
selfDrawnEx = mkHand1
  [ mkSequence' c c7
  , mkTriplet' r we
  , mkSequence' r k1
  , mkSequence' c b4
  , mkPair' c ww
  ]
  [f1, s2]
  Nothing

allSimplesEx1 :: Maybe Hand
allSimplesEx1 = mkHand1
  [ mkSequence' r b4
  , mkSequence' r c6
  , mkTriplet' c b3
  , mkTriplet' r k6
  , mkPair' c b2
  ]
  []
  Nothing

allSimplesEx2 :: Maybe Hand
allSimplesEx2 = mkHand1
  [ mkPair' c c2
  , mkPair' c c5
  , mkPair' c c8
  , mkPair' c b3
  , mkPair' c b5
  , mkPair' c k7
  , mkPair' r k8
  ]
  [s3]
  Nothing

allTypesEx1 :: Maybe Hand
allTypesEx1 = mkHand1
  [ mkSequence' r c7
  , mkTriplet' r ws
  , mkTriplet' c k4
  , mkSequence' c b6
  , mkPair' r dw
  ]
  [f1]
  Nothing

allTypesEx2 :: Maybe Hand
allTypesEx2 = mkHand1
  [ mkPair' c c7
  , mkPair' c ws
  , mkPair' c k4
  , mkPair' c b6
  , mkPair' c dr
  , mkPair' c dw
  , mkPair' r ww
  ]
  [f1]
  Nothing

illegalCallEx :: Maybe Hand
illegalCallEx = Nothing



-- | 2.0 Triplets and Quartets

allTripletsEx :: Maybe Hand
allTripletsEx = mkHand1
  [ mkTriplet' r b4
  , mkTriplet' r dg
  , mkQuartet' c k6
  , mkTriplet' c dw
  , mkPair' r b3
  ]
  [f3]
  Nothing

twoConcealedTripletsEx :: Maybe Hand
twoConcealedTripletsEx = mkHand1
  [ mkTriplet' c b4
  , mkTriplet' c c3
  , mkSequence' r k2
  , mkPair' c ws
  , mkSequence' r k6
  ]
  [s2]
  Nothing

threeConcealedTripletsEx :: Maybe Hand
threeConcealedTripletsEx = mkHand1
  [ mkTriplet' c b4
  , mkQuartet' c c3
  , mkSequence' r k2
  , mkPair' c ws
  , mkTriplet' c k6
  ]
  [s2]
  Nothing

fourConcealedTripletsEx1 :: Maybe Hand
fourConcealedTripletsEx1 = mkHand1
  [ mkTriplet' c b4
  , mkTriplet' c c3
  , mkTriplet' c k2
  , mkPair' c ws
  , mkTriplet' c k6
  ]
  [s2]
  Nothing

fourConcealedTripletsEx2 :: Maybe Hand
fourConcealedTripletsEx2 = mkHand1
  [ mkTriplet' c b4
  , mkTriplet' c c4
  , mkTriplet' c k4
  , mkPair' c ws
  , mkTriplet' c k6
  ]
  [s2]
  Nothing

oneQuartetEx :: Maybe Hand
oneQuartetEx = mkHand1
  [ mkQuartet' r c3
  , mkSequence' c b2
  , mkTriplet' r b9
  , mkPair' c dg
  , mkSequence' r k2
  ]
  []
  Nothing

twoQuartetsEx :: Maybe Hand
twoQuartetsEx = mkHand1
  [ mkQuartet' r c3
  , mkSequence' c b2
  , mkTriplet' r b9 >>= flip promoteTriplet b9
  , mkPair' c dg
  , mkSequence' r k2
  ]
  []
  Nothing

threeQuartetsEx :: Maybe Hand
threeQuartetsEx = mkHand1
  [ mkQuartet' r c3
  , mkSequence' c b2
  , mkTriplet' r b9 >>= flip promoteTriplet b9
  , mkPair' c dg
  , mkQuartet' r k5
  ]
  []
  Nothing

fourQuartetsEx :: Maybe Hand
fourQuartetsEx = mkHand1
  [ mkQuartet' r c3
  , mkQuartet' c b6
  , mkTriplet' r b9 >>= flip promoteTriplet b9
  , mkPair' c dg
  , mkTriplet' r k5 >>= flip promoteTriplet k5
  ]
  []
  Nothing



-- | 3.0 Identical Sequences

twoIdenticalSequencesEx :: Maybe Hand
twoIdenticalSequencesEx = mkHand1
  [ mkSequence' r b4
  , mkSequence' c c6
  , mkSequence' r c6
  , mkPair' c k3
  , mkTriplet' r we
  ]
  [s1]
  Nothing


twoIdenticalSequencesTwiceEx :: Maybe Hand
twoIdenticalSequencesTwiceEx = mkHand1
  [ mkSequence' r b4
  , mkSequence' c c6
  , mkSequence' r c6
  , mkPair' c k3
  , mkSequence' r b4
  ]
  [s1]
  Nothing

threeIdenticalSequencesEx :: Maybe Hand
threeIdenticalSequencesEx = mkHand1
  [ mkSequence' r b4
  , mkSequence' r b4
  , mkSequence' c c6
  , mkPair' c k3
  , mkSequence' r b4
  ]
  [s1]
  Nothing

fourIdenticalSequencesEx :: Maybe Hand
fourIdenticalSequencesEx = mkHand1
  [ mkSequence' r b4
  , mkSequence' r b4
  , mkSequence' c b4
  , mkPair' c k3
  , mkSequence' r b4
  ]
  [s1]
  Nothing


-- | 4.0 Similar Sets

threeSimilarSequencesEx :: Maybe Hand
threeSimilarSequencesEx = mkHand1
  [ mkSequence' r k3
  , mkSequence' r b3
  , mkTriplet' r dw
  , mkPair' c b7
  , mkSequence' r c3
  ]
  [s2]
  Nothing


littleThreeSimilarTripletsEx :: Maybe Hand
littleThreeSimilarTripletsEx = mkHand1
  [ mkTriplet' r c6
  , mkQuartet' r k6
  , mkSequence' c c1
  , mkPair' r b6
  , mkTriplet' r dg
  ]
  [s4]
  Nothing

threeSimilarTripletsEx :: Maybe Hand
threeSimilarTripletsEx = mkHand1
  [ mkTriplet' r c6
  , mkQuartet' r k6
  , mkSequence' c c1
  , mkTriplet' r b6 >>= flip promoteTriplet b6
  , mkPair' r dg
  ]
  [s4]
  Nothing



-- | 5.0 Consecutive Sets

threeConsecutiveSequencesEx1 :: Maybe Hand
threeConsecutiveSequencesEx1 = mkHand1
  [ mkSequence' r c1
  , mkSequence' r c2
  , mkSequence' c c3
  , mkTriplet' c b8
  , mkPair' r dg
  ]
  [s1]
  Nothing

threeConsecutiveSequencesEx2 :: Maybe Hand
threeConsecutiveSequencesEx2 = mkHand1
  [ mkSequence' r b1
  , mkSequence' r b3
  , mkSequence' c b5
  , mkTriplet' c k8
  , mkPair' r dg
  ]
  [s1]
  Nothing

nineTileStraightEx :: Maybe Hand
nineTileStraightEx = mkHand1
  [ mkSequence' r b1
  , mkSequence' r b4
  , mkTriplet' c ww
  , mkPair' c k2
  , mkSequence' r b7
  ]
  [f2]
  Nothing

threeConsecutiveSequencesTwiceEx1 :: Maybe Hand
threeConsecutiveSequencesTwiceEx1 = mkHand1
  [ mkSequence' r c2
  , mkSequence' r c3
  , mkSequence' c c4
  , mkSequence' c c6
  , mkPair' r b2
  ]
  [f1, f3, s2]
  Nothing

threeConsecutiveSequencesTwiceEx2 :: Maybe Hand
threeConsecutiveSequencesTwiceEx2 = mkHand1
  [ mkSequence' r c2
  , mkSequence' r c4
  , mkSequence' c c5
  , mkSequence' c c6
  , mkPair' r k7
  ]
  []
  Nothing

fourConsecutiveSequencesEx1 :: Maybe Hand
fourConsecutiveSequencesEx1 = mkHand1
  [ mkSequence' r c1
  , mkSequence' r c2
  , mkSequence' c c3
  , mkSequence' c c4
  , mkPair' r b2
  ]
  [f1]
  Nothing

fourConsecutiveSequencesEx2 :: Maybe Hand
fourConsecutiveSequencesEx2 = mkHand1
  [ mkSequence' r b1
  , mkSequence' r b3
  , mkSequence' c b5
  , mkSequence' c b7
  , mkPair' r k2
  ]
  [f4, s3]
  Nothing


threeConsecutiveTripletsEx :: Maybe Hand
threeConsecutiveTripletsEx = mkHand1
  [ mkTriplet' r k5
  , mkTriplet' r k6
  , mkSequence' c c2
  , mkPair' c dg
  , mkTriplet' r k7
  ]
  []
  Nothing

fourConsecutiveTripletsEx1 :: Maybe Hand
fourConsecutiveTripletsEx1 = mkHand1
  [ mkTriplet' r k5
  , mkTriplet' r k6
  , mkTriplet' r k7
  , mkPair' c c8
  , mkTriplet' r k8
  ]
  [f2]
  Nothing

fourConsecutiveTripletsEx2 :: Maybe Hand
fourConsecutiveTripletsEx2 = mkHand1
  [ mkTriplet' r k5
  , mkTriplet' r k6
  , mkTriplet' r k7
  , mkPair' c dg
  , mkTriplet' r k8
  ]
  []
  Nothing

fourConsecutiveTripletsEx3 :: Maybe Hand
fourConsecutiveTripletsEx3 = mkHand1
  [ mkTriplet' r k5
  , mkTriplet' r k6
  , mkTriplet' r k7
  , mkPair' c k2
  , mkTriplet' r k8
  ]
  []
  Nothing

threeMothersEx :: Maybe Hand
threeMothersEx = mkHand1
  [ mkTriplet' r k5
  , mkTriplet' r k6
  , mkSequence' r k5
  , mkPair' c dg
  , mkTriplet' r k7
  ]
  []
  Nothing



-- | 6.0 Suit Patterns

mixedOneSuitEx1 :: Maybe Hand
mixedOneSuitEx1 = mkHand1
  [ mkTriplet' r k5
  , mkTriplet' r we
  , mkSequence' r k2
  , mkPair' c dg
  , mkSequence' r k7
  ]
  []
  Nothing

mixedOneSuitEx2 :: Maybe Hand
mixedOneSuitEx2 = mkHand1
  [ mkPair' c c2
  , mkPair' c c5
  , mkPair' c c7
  , mkPair' c c8
  , mkPair' c ww
  , mkPair' c wn
  , mkPair' r dr
  ]
  [s3]
  Nothing

pureOneSuitEx1 :: Maybe Hand
pureOneSuitEx1 = mkHand1
  [ mkTriplet' r k1
  , mkSequence' r k3
  , mkSequence' r k4
  , mkPair' c k9
  , mkTriplet' r k8
  ]
  [f3]
  Nothing

pureOneSuitEx2 :: Maybe Hand
pureOneSuitEx2 = mkHand1
  [ mkPair' c c1
  , mkPair' c c2
  , mkPair' c c4
  , mkPair' c c5
  , mkPair' c c7
  , mkPair' c c8
  , mkPair' r c9
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
  [ mkSequence' r b1
  , mkSequence' r b7
  , mkTriplet' c k3
  , mkPair' c ws
  , mkSequence' r c6
  ]
  [s1]
  Nothing

twoTailedTerminalSequencesEx2 :: Maybe Hand
twoTailedTerminalSequencesEx2 = mkHand1
  [ mkSequence' r b1
  , mkSequence' r b7
  , mkSequence' c k1
  , mkPair' c c3
  , mkSequence' r k7
  ]
  [f2]
  Nothing

twoTailedTerminalSequencesEx3 :: Maybe Hand
twoTailedTerminalSequencesEx3 = mkHand1
  [ mkSequence' r b1
  , mkSequence' r b7
  , mkSequence' c b1
  , mkPair' c c3
  , mkSequence' r b7
  ]
  [f2]
  Nothing

twoTailedTerminalTripletsEx1 :: Maybe Hand
twoTailedTerminalTripletsEx1 = mkHand1
  [ mkTriplet' r b1
  , mkTriplet' r b9
  , mkTriplet' c we
  , mkPair' c c3
  , mkSequence' r k4
  ]
  []
  Nothing

twoTailedTerminalTripletsEx2 :: Maybe Hand
twoTailedTerminalTripletsEx2 = mkHand1
  [ mkTriplet' r b1
  , mkTriplet' r b9
  , mkTriplet' c k1
  , mkPair' c c3
  , mkTriplet' r k9
  ]
  [s3]
  Nothing

twoTailedTerminalsEx1 :: Maybe Hand
twoTailedTerminalsEx1 = mkHand1
  [ mkTriplet' r b1
  , mkTriplet' r b9
  , mkSequence' c b1
  , mkPair' c c3
  , mkSequence' r b7
  ]
  [s1]
  Nothing

twoTailedTerminalsEx2 :: Maybe Hand
twoTailedTerminalsEx2 = mkHand1
  [ mkTriplet' r b1
  , mkTriplet' r b9
  , mkSequence' c b1
  , mkPair' c ww
  , mkSequence' r b7
  ]
  [s1]
  Nothing

twoTailedTerminalsEx3 :: Maybe Hand
twoTailedTerminalsEx3 = mkHand1
  [ mkTriplet' r b1
  , mkTriplet' r b9
  , mkSequence' c b1
  , mkPair' c c9
  , mkSequence' r b7
  ]
  [s1]
  Nothing

twoTailedTerminalsEx4 :: Maybe Hand
twoTailedTerminalsEx4 = mkHand1
  [ mkTriplet' r b1
  , mkTriplet' r b9
  , mkSequence' c b1
  , mkPair' c b5
  , mkSequence' r b7
  ]
  [s1]
  Nothing


mixedLesserTerminalsEx :: Maybe Hand
mixedLesserTerminalsEx = mkHand1
  [ mkSequence' r c1
  , mkTriplet' r c9
  , mkSequence' c b1
  , mkTriplet' c k9
  , mkPair' r ws
  ]
  []
  Nothing

pureLesserTerminalsEx :: Maybe Hand
pureLesserTerminalsEx = mkHand1
  [ mkSequence' r c1
  , mkTriplet' r c9
  , mkSequence' r b1
  , mkTriplet' c k1
  , mkPair' c b9
  ]
  [f3]
  Nothing

mixedGreaterTerminalsEx1 :: Maybe Hand
mixedGreaterTerminalsEx1 = mkHand1
  [ mkTriplet' r c1
  , mkTriplet' r c9
  , mkTriplet' r we
  , mkTriplet' c dr
  , mkPair' c b9
  ]
  [f1, s2]
  Nothing

mixedGreaterTerminalsEx2 :: Maybe Hand
mixedGreaterTerminalsEx2 = mkHand1
  [ mkPair' c c9
  , mkPair' c b9
  , mkPair' c k1
  , mkPair' c ws
  , mkPair' c wn
  , mkPair' c dg
  , mkPair' r we
  ]
  []
  Nothing

pureSuitTerminalsEx1 :: Maybe Hand
pureSuitTerminalsEx1 = mkHand1
  [ mkSequence' r b1
  , mkSequence' r b7
  , mkSequence' c b1
  , mkPair' c b1
  , mkSequence' r b7
  ]
  [f1]
  Nothing

pureSuitTerminalsEx2 :: Maybe Hand
pureSuitTerminalsEx2 = mkHand1
  [ mkSequence' r k1
  , mkSequence' r k1
  , mkSequence' c k1
  , mkPair' c k9
  , mkSequence' r k7
  ]
  [f1]
  Nothing

pureSuitTerminalsEx3 :: Maybe Hand
pureSuitTerminalsEx3 = mkHand1
  [ mkTriplet' r k1
  , mkSequence' c k1
  , mkSequence' r k7
  , mkPair' c k9
  , mkSequence' r k7
  ]
  [s4]
  Nothing

pureSuitTerminalsEx4 :: Maybe Hand
pureSuitTerminalsEx4 = mkHand1
  [ mkSequence' r k1
  , mkSequence' c k1
  , mkSequence' r k7
  , mkPair' c k1
  , mkTriplet' r k9
  ]
  [f4]
  Nothing

pureGreaterTerminalsEx1 :: Maybe Hand
pureGreaterTerminalsEx1 = mkHand1
  [ mkTriplet' r c1
  , mkTriplet' r c9
  , mkTriplet' r b1
  , mkQuartet' c k1
  , mkPair' c b9
  ]
  [s3]
  Nothing

pureGreaterTerminalsEx2 :: Maybe Hand
pureGreaterTerminalsEx2 = mkHand1
  [ mkPair' c c1
  , mkPair' c c9
  , mkPair' c b1
  , mkPair' c b9
  , mkPair' c k1
  , mkPair' c k9
  , mkPair' r k1
  ]
  []
  Nothing



-- | 8.0 Honor Tiles

windTripletEx :: Maybe Hand
windTripletEx = mkHand1
  [ mkTriplet' r c8
  , mkSequence' r b3
  , mkSequence' r k7
  , mkTriplet' r ww
  , mkPair' c ws
  ]
  [s2]
  Nothing

littleThreeWindsEx :: Maybe Hand
littleThreeWindsEx = mkHand1
  [ mkTriplet' r ws
  , mkTriplet' r wn
  , mkSequence' r c3
  , mkQuartet' c k7
  , mkPair' c we
  ]
  [f1, f3]
  Nothing

bigThreeWindsEx :: Maybe Hand
bigThreeWindsEx = mkHand1
  [ mkTriplet' r ws
  , mkTriplet' r wn
  , mkSequence' r c3
  , mkPair' c k7
  , mkTriplet' c we
  ]
  [s1]
  Nothing

littleFourWindsEx :: Maybe Hand
littleFourWindsEx = mkHand1
  [ mkTriplet' r ws
  , mkTriplet' c ww
  , mkTriplet' r wn
  , mkSequence' r c3
  , mkPair' c we
  ]
  [f3, s2]
  Nothing

bigFourWindsEx :: Maybe Hand
bigFourWindsEx = mkHand1
  [ mkTriplet' r ws
  , mkTriplet' c ww
  , mkTriplet' r wn
  , mkPair' c k7
  , mkTriplet' r we
  ]
  [f4, s2]
  Nothing


dragonTripletEx1 :: Maybe Hand
dragonTripletEx1 = mkHand1
  [ mkSequence' r c1
  , mkSequence' r k2
  , mkTriplet' r dr
  , mkPair' c dw
  , mkTriplet' r ww
  ]
  []
  Nothing

dragonTripletEx2 :: Maybe Hand
dragonTripletEx2 = mkHand1
  [ mkSequence' r c1
  , mkSequence' r k2
  , mkTriplet' r dr
  , mkPair' c ww
  , mkTriplet' r dg
  ]
  []
  Nothing

littleThreeDragonsEx :: Maybe Hand
littleThreeDragonsEx = mkHand1
  [ mkTriplet' r dr
  , mkTriplet' r dg
  , mkSequence' c b3
  , mkTriplet' c k8
  , mkPair' r dw
  ]
  [s3]
  Nothing

bigThreeDragonsEx :: Maybe Hand
bigThreeDragonsEx = mkHand1
  [ mkTriplet' r dr
  , mkTriplet' r dg
  , mkSequence' c b3
  , mkPair' c k8
  , mkTriplet' r dw
  ]
  [f2]
  Nothing


allHonorsEx1 :: Maybe Hand
allHonorsEx1 = mkHand1
  [ mkTriplet' r we
  , mkTriplet' r wn
  , mkTriplet' r dr
  , mkTriplet' c dg
  , mkTriplet' r ww
  ]
  [s1]
  Nothing

allHonorsEx2 :: Maybe Hand
allHonorsEx2 = mkHand1
  [ mkPair' c we
  , mkPair' c ww
  , mkPair' c ww
  , mkPair' c wn
  , mkPair' c dr
  , mkPair' c dg
  , mkPair' r we
  ]
  [s3]
  Nothing

allHonorPairsEx :: Maybe Hand
allHonorPairsEx = mkHand1
  [ mkPair' c we
  , mkPair' c ws
  , mkPair' c ww
  , mkPair' c wn
  , mkPair' c dr
  , mkPair' c dg
  , mkPair' r dw
  ]
  [s3]
  Nothing



-- | 9.0 Color Hands

allGreenEx1 :: Maybe Hand
allGreenEx1 = mkHand1
  [ mkSequence' r b2
  , mkSequence' r b2
  , mkTriplet' r b6
  , mkTriplet' c dg
  , mkPair' c b8
  ]
  [f2, s2]
  Nothing

allGreenEx2 :: Maybe Hand
allGreenEx2 = mkHand1
  [ mkPair' c b2
  , mkPair' c b3
  , mkPair' c b4
  , mkPair' c b6
  , mkPair' c b8
  , mkPair' c dg
  , mkPair' r dg
  ]
  [f3]
  Nothing

allRedEx :: Maybe Hand
allRedEx = mkHand1
  [ mkTriplet' r b1
  , mkTriplet' r b5
  , mkTriplet' c b7
  , mkTriplet' c b9
  , mkPair' c dr
  ]
  [f1, s1]
  Nothing

allBlueEx :: Maybe Hand
allBlueEx = mkHand1
  [ mkTriplet' c c8
  , mkTriplet' r ws
  , mkTriplet' r ww
  , mkTriplet' c dw
  , mkPair' r ww
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
  [ mkPair' c c2
  , mkPair' c c5
  , mkPair' c c8
  , mkPair' c b3
  , mkPair' c we
  , mkPair' c k7
  , mkPair' r k8
  ]
  [s3]
  Nothing

sevenPairsEx2 :: Maybe Hand
sevenPairsEx2 = mkHand1
  [ mkPair' c c2
  , mkPair' c c2
  , mkPair' c c8
  , mkPair' c b3
  , mkPair' c b5
  , mkPair' c dw
  , mkPair' r k8
  ]
  [s3]
  Nothing

sevenConsecutivePairsEx1 :: Maybe Hand
sevenConsecutivePairsEx1 = mkHand1
  [ mkPair' c c1
  , mkPair' c c2
  , mkPair' c c3
  , mkPair' c c4
  , mkPair' c c5
  , mkPair' c c6
  , mkPair' r c7
  ]
  [s3]
  Nothing

sevenConsecutivePairsEx2 :: Maybe Hand
sevenConsecutivePairsEx2 = mkHand1
  [ mkPair' c b3
  , mkPair' c b4
  , mkPair' c b5
  , mkPair' c b6
  , mkPair' c b8
  , mkPair' c b9
  , mkPair' r b7
  ]
  [f4, s4]
  Nothing

grandChariotEx :: Maybe Hand
grandChariotEx = mkHand1
  [ mkPair' c c2
  , mkPair' c c3
  , mkPair' c c4
  , mkPair' c c5
  , mkPair' c c6
  , mkPair' c c8
  , mkPair' r c7
  ]
  [s3]
  Nothing

bambooForestEx :: Maybe Hand
bambooForestEx = mkHand1
  [ mkPair' c b2
  , mkPair' c b3
  , mkPair' c b4
  , mkPair' c b6
  , mkPair' c b7
  , mkPair' c b8
  , mkPair' r b5
  ]
  [f3]
  Nothing

numerousNeighborsEx :: Maybe Hand
numerousNeighborsEx = mkHand1
  [ mkPair' c k2
  , mkPair' c k3
  , mkPair' c k4
  , mkPair' c k5
  , mkPair' c k7
  , mkPair' c k8
  , mkPair' r k6
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
  [ mkSequence' r c2
  , mkTriplet' r b2
  , mkSequence' r b7
  , mkTriplet' c wn
  , mkPair' r dr
  ]
  [f2]
  Nothing

fourFlowersEx :: Maybe Hand
fourFlowersEx = mkHand1
  [ mkSequence' r b7
  , mkTriplet' r k4
  , mkSequence' r k7
  , mkTriplet' c dw
  , mkPair' r ww
  ]
  [f1, f2, f3, f4]
  Nothing

fourSeasonsEx :: Maybe Hand
fourSeasonsEx = mkHand1
  [ mkSequence' r c3
  , mkTriplet' r b4
  , mkSequence' r k3
  , mkTriplet' c wn
  , mkPair' r ww
  ]
  [s1, s2, s3, s4]
  Nothing

allBonusTilesEx :: Maybe Hand
allBonusTilesEx = mkHand1
  [ mkSequence' r c7
  , mkTriplet' r b2
  , mkSequence' r b7
  , mkTriplet' c k4
  , mkPair' r ww
  ]
  [f1, f2, f3, f4, s1, s2, s3, s4]
  Nothing

