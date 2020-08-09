-- | Static melds and collections
module Game.Mahjong.Static.Melds (
  -- ** meld aliases
  c123, c234, c345, c456, c567, c678, c789,
  c111, c222, c333, c444, c555, c666, c777, c888, c999,
  c1111, c2222, c3333, c4444, c5555, c6666, c7777, c8888, c9999,
  c11, c22, c33, c44, c55, c66, c77, c88, c99,
  b123, b234, b345, b456, b567, b678, b789,
  b111, b222, b333, b444, b555, b666, b777, b888, b999,
  b1111, b2222, b3333, b4444, b5555, b6666, b7777, b8888, b9999,
  b11, b22, b33, b44, b55, b66, b77, b88, b99,
  k123, k234, k345, k456, k567, k678, k789,
  k111, k222, k333, k444, k555, k666, k777,  k888, k999,
  k1111, k2222, k3333, k4444, k5555, k6666, k7777, k8888, k9999,
  k11, k22, k33, k44, k55, k66, k77, k88, k99,
  weee, wsss, wwww, wnnn,
  weeee, wssss, wwwww, wnnnn,
  wee, wss, www, wnn,
  drrr, dggg, dwww,
  drrrr, dgggg, dwwww,
  drr, dgg, dww,

  -- ** meld collections
  coinSequences, bambooSequences, characterSequences, terminalSequences,
  coinTriplets, bambooTriplets, characterTriplets, terminalTriplets, windTriplets, dragonTriplets,
  coinQuartets, bambooQuartets, characterQuartets, terminalQuartets, windQuartets, dragonQuartets,
  coinPairs, bambooPairs, characterPairs, terminalPairs, windPairs, dragonPairs,
) where

import Game.Mahjong.Meld
import Game.Mahjong.Static.Tiles

import Data.Maybe (fromJust)


-------------------------------------------------------------------------------
-- Meld collections
-------------------------------------------------------------------------------

{- Coins -}

c123, c234, c345, c456, c567, c678, c789 :: Meld
c123 = fromJust $ mkMeld Revealed Sequence [c1, c2, c3]
c234 = fromJust $ mkMeld Revealed Sequence [c2, c3, c4]
c345 = fromJust $ mkMeld Revealed Sequence [c3, c4, c5]
c456 = fromJust $ mkMeld Revealed Sequence [c4, c5, c6]
c567 = fromJust $ mkMeld Revealed Sequence [c5, c6, c7]
c678 = fromJust $ mkMeld Revealed Sequence [c6, c7, c8]
c789 = fromJust $ mkMeld Revealed Sequence [c7, c8, c9]

c111, c222, c333, c444, c555, c666, c777, c888, c999 :: Meld
c111 = fromJust $ mkMeld Revealed Triplet [c1, c1, c1]
c222 = fromJust $ mkMeld Revealed Triplet [c2, c2, c2]
c333 = fromJust $ mkMeld Revealed Triplet [c3, c3, c3]
c444 = fromJust $ mkMeld Revealed Triplet [c4, c4, c4]
c555 = fromJust $ mkMeld Revealed Triplet [c5, c5, c5]
c666 = fromJust $ mkMeld Revealed Triplet [c6, c6, c6]
c777 = fromJust $ mkMeld Revealed Triplet [c7, c7, c7]
c888 = fromJust $ mkMeld Revealed Triplet [c8, c8, c8]
c999 = fromJust $ mkMeld Revealed Triplet [c9, c9, c9]

c1111, c2222, c3333, c4444, c5555, c6666, c7777, c8888, c9999 :: Meld
c1111 = fromJust $ mkMeld Revealed Quartet [c1, c1, c1, c1]
c2222 = fromJust $ mkMeld Revealed Quartet [c2, c2, c2, c2]
c3333 = fromJust $ mkMeld Revealed Quartet [c3, c3, c3, c3]
c4444 = fromJust $ mkMeld Revealed Quartet [c4, c4, c4, c4]
c5555 = fromJust $ mkMeld Revealed Quartet [c5, c5, c5, c5]
c6666 = fromJust $ mkMeld Revealed Quartet [c6, c6, c6, c6]
c7777 = fromJust $ mkMeld Revealed Quartet [c7, c7, c7, c7]
c8888 = fromJust $ mkMeld Revealed Quartet [c8, c8, c8, c8]
c9999 = fromJust $ mkMeld Revealed Quartet [c9, c9, c9, c9]

c11, c22, c33, c44, c55, c66, c77, c88, c99 :: Meld
c11 = fromJust $ mkMeld Revealed Pair [c1, c1]
c22 = fromJust $ mkMeld Revealed Pair [c2, c2]
c33 = fromJust $ mkMeld Revealed Pair [c3, c3]
c44 = fromJust $ mkMeld Revealed Pair [c4, c4]
c55 = fromJust $ mkMeld Revealed Pair [c5, c5]
c66 = fromJust $ mkMeld Revealed Pair [c6, c6]
c77 = fromJust $ mkMeld Revealed Pair [c7, c7]
c88 = fromJust $ mkMeld Revealed Pair [c8, c8]
c99 = fromJust $ mkMeld Revealed Pair [c9, c9]

{- Bamboos -}

b123, b234, b345, b456, b567, b678, b789 :: Meld
b123 = fromJust $ mkMeld Revealed Sequence [b1, b2, b3]
b234 = fromJust $ mkMeld Revealed Sequence [b2, b3, b4]
b345 = fromJust $ mkMeld Revealed Sequence [b3, b4, b5]
b456 = fromJust $ mkMeld Revealed Sequence [b4, b5, b6]
b567 = fromJust $ mkMeld Revealed Sequence [b5, b6, b7]
b678 = fromJust $ mkMeld Revealed Sequence [b6, b7, b8]
b789 = fromJust $ mkMeld Revealed Sequence [b7, b8, b9]

b111, b222, b333, b444, b555, b666, b777, b888, b999 :: Meld
b111 = fromJust $ mkMeld Revealed Triplet [b1, b1, b1]
b222 = fromJust $ mkMeld Revealed Triplet [b2, b2, b2]
b333 = fromJust $ mkMeld Revealed Triplet [b3, b3, b3]
b444 = fromJust $ mkMeld Revealed Triplet [b4, b4, b4]
b555 = fromJust $ mkMeld Revealed Triplet [b5, b5, b5]
b666 = fromJust $ mkMeld Revealed Triplet [b6, b6, b6]
b777 = fromJust $ mkMeld Revealed Triplet [b7, b7, b7]
b888 = fromJust $ mkMeld Revealed Triplet [b8, b8, b8]
b999 = fromJust $ mkMeld Revealed Triplet [b9, b9, b9]

b1111, b2222, b3333, b4444, b5555, b6666, b7777, b8888, b9999 :: Meld
b1111 = fromJust $ mkMeld Revealed Quartet [b1, b1, b1, b1]
b2222 = fromJust $ mkMeld Revealed Quartet [b2, b2, b2, b2]
b3333 = fromJust $ mkMeld Revealed Quartet [b3, b3, b3, b3]
b4444 = fromJust $ mkMeld Revealed Quartet [b4, b4, b4, b4]
b5555 = fromJust $ mkMeld Revealed Quartet [b5, b5, b5, b5]
b6666 = fromJust $ mkMeld Revealed Quartet [b6, b6, b6, b6]
b7777 = fromJust $ mkMeld Revealed Quartet [b7, b7, b7, b7]
b8888 = fromJust $ mkMeld Revealed Quartet [b8, b8, b8, b8]
b9999 = fromJust $ mkMeld Revealed Quartet [b9, b9, b9, b9]

b11, b22, b33, b44, b55, b66, b77, b88, b99 :: Meld
b11 = fromJust $ mkMeld Revealed Pair [b1, b1]
b22 = fromJust $ mkMeld Revealed Pair [b2, b2]
b33 = fromJust $ mkMeld Revealed Pair [b3, b3]
b44 = fromJust $ mkMeld Revealed Pair [b4, b4]
b55 = fromJust $ mkMeld Revealed Pair [b5, b5]
b66 = fromJust $ mkMeld Revealed Pair [b6, b6]
b77 = fromJust $ mkMeld Revealed Pair [b7, b7]
b88 = fromJust $ mkMeld Revealed Pair [b8, b8]
b99 = fromJust $ mkMeld Revealed Pair [b9, b9]

{- Characters -}

k123, k234, k345, k456, k567, k678, k789 :: Meld
k123 = fromJust $ mkMeld Revealed Sequence [k1, k2, k3]
k234 = fromJust $ mkMeld Revealed Sequence [k2, k3, k4]
k345 = fromJust $ mkMeld Revealed Sequence [k3, k4, k5]
k456 = fromJust $ mkMeld Revealed Sequence [k4, k5, k6]
k567 = fromJust $ mkMeld Revealed Sequence [k5, k6, k7]
k678 = fromJust $ mkMeld Revealed Sequence [k6, k7, k8]
k789 = fromJust $ mkMeld Revealed Sequence [k7, k8, k9]

k111, k222, k333, k444, k555, k666, k777,  k888, k999 :: Meld
k111 = fromJust $ mkMeld Revealed Triplet [k1, k1, k1]
k222 = fromJust $ mkMeld Revealed Triplet [k2, k2, k2]
k333 = fromJust $ mkMeld Revealed Triplet [k3, k3, k3]
k444 = fromJust $ mkMeld Revealed Triplet [k4, k4, k4]
k555 = fromJust $ mkMeld Revealed Triplet [k5, k5, k5]
k666 = fromJust $ mkMeld Revealed Triplet [k6, k6, k6]
k777 = fromJust $ mkMeld Revealed Triplet [k7, k7, k7]
k888 = fromJust $ mkMeld Revealed Triplet [k8, k8, k8]
k999 = fromJust $ mkMeld Revealed Triplet [k9, k9, k9]

k1111, k2222, k3333, k4444, k5555, k6666, k7777, k8888, k9999 :: Meld
k1111 = fromJust $ mkMeld Revealed Quartet [k1, k1, k1, k1]
k2222 = fromJust $ mkMeld Revealed Quartet [k2, k2, k2, k2]
k3333 = fromJust $ mkMeld Revealed Quartet [k3, k3, k3, k3]
k4444 = fromJust $ mkMeld Revealed Quartet [k4, k4, k4, k4]
k5555 = fromJust $ mkMeld Revealed Quartet [k5, k5, k5, k5]
k6666 = fromJust $ mkMeld Revealed Quartet [k6, k6, k6, k6]
k7777 = fromJust $ mkMeld Revealed Quartet [k7, k7, k7, k7]
k8888 = fromJust $ mkMeld Revealed Quartet [k8, k8, k8, k8]
k9999 = fromJust $ mkMeld Revealed Quartet [k9, k9, k9, k9]

k11, k22, k33, k44, k55, k66, k77, k88, k99 :: Meld
k11 = fromJust $ mkMeld Revealed Pair [k1, k1]
k22 = fromJust $ mkMeld Revealed Pair [k2, k2]
k33 = fromJust $ mkMeld Revealed Pair [k3, k3]
k44 = fromJust $ mkMeld Revealed Pair [k4, k4]
k55 = fromJust $ mkMeld Revealed Pair [k5, k5]
k66 = fromJust $ mkMeld Revealed Pair [k6, k6]
k77 = fromJust $ mkMeld Revealed Pair [k7, k7]
k88 = fromJust $ mkMeld Revealed Pair [k8, k8]
k99 = fromJust $ mkMeld Revealed Pair [k9, k9]

{- Winds -}

weee, wsss, wwww, wnnn :: Meld
weee = fromJust $ mkMeld Revealed Triplet [we, we, we]
wsss = fromJust $ mkMeld Revealed Triplet [ws, ws, ws]
wwww = fromJust $ mkMeld Revealed Triplet [ww, ww, ww]
wnnn = fromJust $ mkMeld Revealed Triplet [wn, wn, wn]

weeee, wssss, wwwww, wnnnn :: Meld
weeee = fromJust $ mkMeld Revealed Quartet [we, we, we, we]
wssss = fromJust $ mkMeld Revealed Quartet [ws, ws, ws, ws]
wwwww = fromJust $ mkMeld Revealed Quartet [ww, ww, ww, ww]
wnnnn = fromJust $ mkMeld Revealed Quartet [wn, wn, wn, wn]

wee, wss, www, wnn :: Meld
wee = fromJust $ mkMeld Revealed Pair [we, we]
wss = fromJust $ mkMeld Revealed Pair [ws, ws]
www = fromJust $ mkMeld Revealed Pair [ww, ww]
wnn = fromJust $ mkMeld Revealed Pair [wn, wn]

{- Dragons -}

drrr, dggg, dwww :: Meld
drrr = fromJust $ mkMeld Revealed Triplet [dr, dr, dr]
dggg = fromJust $ mkMeld Revealed Triplet [dg, dg, dg]
dwww = fromJust $ mkMeld Revealed Triplet [dw, dw, dw]

drrrr, dgggg, dwwww :: Meld
drrrr = fromJust $ mkMeld Revealed Quartet [dr, dr, dr, dr]
dgggg = fromJust $ mkMeld Revealed Quartet [dg, dg, dg, dg]
dwwww = fromJust $ mkMeld Revealed Quartet [dw, dw, dw, dw]

drr, dgg, dww :: Meld
drr = fromJust $ mkMeld Revealed Pair [dr, dr]
dgg = fromJust $ mkMeld Revealed Pair [dg, dg]
dww = fromJust $ mkMeld Revealed Pair [dw, dw]


-------------------------------------------------------------------------------
-- Meld collections
-------------------------------------------------------------------------------

{- Sequences -}

coinSequences :: [Meld]
coinSequences = [c123, c234, c345, c456, c567, c678, c789]

bambooSequences :: [Meld]
bambooSequences = [b123, b234, b345, b456, b567, b678, b789]

characterSequences :: [Meld]
characterSequences = [k123, k234, k345, k456, k567, k678, k789]

terminalSequences :: [Meld]
terminalSequences = [c123, c789, b123, b789, k123, k789]

{- Triplets -}

coinTriplets :: [Meld]
coinTriplets = [c111, c222, c333, c444, c555, c666, c777, c888, c999]

bambooTriplets :: [Meld]
bambooTriplets = [b111, b222, b333, b444, b555, b666, b777, b888, b999]

characterTriplets :: [Meld]
characterTriplets = [k111, k222, k333, k444, k555, k666, k777, k888, k999]

terminalTriplets :: [Meld]
terminalTriplets = [c111, c999, b111, b999, k111, k999]

windTriplets :: [Meld]
windTriplets = [weee, wsss, wwww, wnnnn]

dragonTriplets :: [Meld]
dragonTriplets = [drrr, dggg, dwww]

{- Quartets -}

coinQuartets :: [Meld]
coinQuartets = [c1111, c2222, c3333, c4444, c5555, c6666, c7777, c8888, c9999]

bambooQuartets :: [Meld]
bambooQuartets = [b1111, b2222, b3333, b4444, b5555, b6666, b7777, b8888, b9999]

characterQuartets :: [Meld]
characterQuartets = [k1111, k2222, k3333, k4444, k5555, k6666, k7777, k8888, k9999]

terminalQuartets :: [Meld]
terminalQuartets = [c1111, c9999, b1111, b9999, k1111, k9999]

windQuartets :: [Meld]
windQuartets = [weeee, wssss, wwwww, wnnnn]

dragonQuartets :: [Meld]
dragonQuartets = [drrrr, dgggg, dwwww]

{- Pairs -}

coinPairs :: [Meld]
coinPairs = [c11, c22, c33, c44, c55, c66, c77, c88, c99]

bambooPairs :: [Meld]
bambooPairs = [b11, b22, b33, b44, b55, b66, b77, b88, b99]

characterPairs :: [Meld]
characterPairs = [k11, k22, k33, k44, k55, k66, k77, k88, k99]

terminalPairs :: [Meld]
terminalPairs = [c11, c99, b11, b99, k11, k99]

windPairs :: [Meld]
windPairs = [wee, wss, www, wnn]

dragonPairs :: [Meld]
dragonPairs = [drr, dgg, dww]
