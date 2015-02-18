-- |
-- Module      :  Game.Mahjong.Tile.Collections
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definition of tiles
--   along with tile related functions
module Game.Mahjong.Tile.Collections where

import Game.Mahjong.Internal.Tile


-------------------------------------------------------------------------------

{- Tile Aliases -}

c1, c2, c3, c4, c5, c6, c7, c8, c9 :: Tile Suit
c1 = CTile One
c2 = CTile Two
c3 = CTile Three
c4 = CTile Four
c5 = CTile Five
c6 = CTile Six
c7 = CTile Seven
c8 = CTile Eight
c9 = CTile Nine

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: Tile Suit
b1 = BTile One
b2 = BTile Two
b3 = BTile Three
b4 = BTile Four
b5 = BTile Five
b6 = BTile Six
b7 = BTile Seven
b8 = BTile Eight
b9 = BTile Nine

k1, k2, k3, k4, k5, k6, k7, k8, k9 :: Tile Suit
k1 = KTile One
k2 = KTile Two
k3 = KTile Three
k4 = KTile Four
k5 = KTile Five
k6 = KTile Six
k7 = KTile Seven
k8 = KTile Eight
k9 = KTile Nine

w1, w2, w3, w4 :: Tile Honor
w1 = WTile East
w2 = WTile South
w3 = WTile West
w4 = WTile North

d1, d2, d3 :: Tile Honor
d1 = DTile Red
d2 = DTile Green
d3 = DTile White

f1, f2, f3, f4 :: Tile Bonus
f1 = FTile PlumBlossom
f2 = FTile Orchid
f3 = FTile Chrysanthemum
f4 = FTile BambooTree

s1, s2, s3, s4 :: Tile Bonus
s1 = STile Spring
s2 = STile Summer
s3 = STile Autumn
s4 = STile Winter

a1, a2, a3, a4 :: Tile Bonus
a1 = ATile Cat
a2 = ATile Mouse
a3 = ATile Cockerel
a4 = ATile Centipede


-------------------------------------------------------------------------------

{- Tile collections -}

coins, bamboos, characters, winds, dragons, flowers, seasons, animals :: Tiles
coins      = map (Wrap . CTile) [One ..]
bamboos    = map (Wrap . BTile) [One ..]
characters = map (Wrap . KTile) [One ..]
winds      = map (Wrap . WTile) [East ..]
dragons    = map (Wrap . DTile) [Red ..]
flowers    = map (Wrap . FTile) [PlumBlossom ..]
seasons    = map (Wrap . STile) [Spring ..]
animals    = map (Wrap . ATile) [Cat ..]

simples, terminals, suits, honors, edges, bonuses, extras :: Tiles
simples    = concatMap (\x -> tail . init $ x) [coins, bamboos, characters]
terminals  = concatMap (\x -> [head x, last x]) [coins, bamboos, characters]
suits      = coins ++ bamboos ++ characters
honors     = winds ++ dragons
edges      = terminals ++ honors
bonuses    = flowers ++ seasons
extras     = bonuses ++ animals

reds, greens, blues :: Tiles
reds       = map (Wrap . BTile) [One, Five, Seven, Nine]
             ++ [Wrap $ DTile Red]
greens     = map (Wrap . BTile) [Two, Three, Four, Six, Eight]
             ++ [Wrap $ DTile Green]
blues      = [Wrap $ CTile Eight] ++ winds ++ [Wrap $ DTile White]

regulars, allTiles :: Tiles
regulars   = coins ++ bamboos ++ characters ++ winds ++ dragons
allTiles   = regulars ++ bonuses

