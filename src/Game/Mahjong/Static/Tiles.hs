-- | Static tiles and collections
module Game.Mahjong.Static.Tiles (
  -- ** Individual tiles
  c1, c2, c3, c4, c5, c6, c7, c8, c9,
  b1, b2, b3, b4, b5, b6, b7, b8, b9,
  k1, k2, k3, k4, k5, k6, k7, k8, k9,
  wn, ws, we, ww,
  dr, dg, dw,
  f1, f2, f3, f4,
  s1, s2, s3, s4,

  -- ** Tile collections
  coins, bamboos, characters, winds, dragons, flowers, seasons,
  simples, terminals, suits, honors, edges, bonuses,
  reds, greens,
  regulars, allTiles,
) where

import Game.Mahjong.Tile


-------------------------------------------------------------------------------
-- Tile Aliases
-------------------------------------------------------------------------------

c1, c2, c3, c4, c5, c6, c7, c8, c9 :: Tile
c1 = mkCoin One
c2 = mkCoin Two
c3 = mkCoin Three
c4 = mkCoin Four
c5 = mkCoin Five
c6 = mkCoin Six
c7 = mkCoin Seven
c8 = mkCoin Eight
c9 = mkCoin Nine

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: Tile
b1 = mkBamboo One
b2 = mkBamboo Two
b3 = mkBamboo Three
b4 = mkBamboo Four
b5 = mkBamboo Five
b6 = mkBamboo Six
b7 = mkBamboo Seven
b8 = mkBamboo Eight
b9 = mkBamboo Nine

k1, k2, k3, k4, k5, k6, k7, k8, k9 :: Tile
k1 = mkCharacter One
k2 = mkCharacter Two
k3 = mkCharacter Three
k4 = mkCharacter Four
k5 = mkCharacter Five
k6 = mkCharacter Six
k7 = mkCharacter Seven
k8 = mkCharacter Eight
k9 = mkCharacter Nine

we, ws, ww, wn :: Tile
we = mkWind East
ws = mkWind South
ww = mkWind West
wn = mkWind North

dr, dg, dw :: Tile
dr = mkDragon Red
dg = mkDragon Green
dw = mkDragon White

f1, f2, f3, f4 :: Tile
f1 = mkFlower PlumBlossom
f2 = mkFlower Orchid
f3 = mkFlower Chrysanthemum
f4 = mkFlower BambooTree

s1, s2, s3, s4 :: Tile
s1 = mkSeason Spring
s2 = mkSeason Summer
s3 = mkSeason Autumn
s4 = mkSeason Winter


-------------------------------------------------------------------------------
-- Tile collections
-------------------------------------------------------------------------------

-- | List of coin tiles.
coins :: [Tile]
coins = map mkCoin [One ..]

-- | List of bamboo tiles.
bamboos :: [Tile]
bamboos = map mkBamboo [One ..]

-- | List of character tiles.
characters :: [Tile]
characters = map mkCharacter [One ..]

-- | List of wind tiles.
winds :: [Tile]
winds = map mkWind [East ..]

-- | List of dragon tiles.
dragons :: [Tile]
dragons = map mkDragon [Red ..]

-- | List of flower tiles.
flowers :: [Tile]
flowers = map mkFlower [PlumBlossom ..]

-- | List of season tiles.
seasons :: [Tile]
seasons = map mkSeason [Spring ..]

-- | List of simple tiles.
simples :: [Tile]
simples = [coins, bamboos, characters] >>= (\x -> tail . init $ x)

-- | List of terminal tiles.
terminals :: [Tile]
terminals = [coins, bamboos, characters] >>= (\x -> [head x, last x])

-- | List of suit tiles.
suits :: [Tile]
suits = coins ++ bamboos ++ characters

-- | List of honor tiles.
honors :: [Tile]
honors = winds ++ dragons

-- | List of edge tiles.
edges :: [Tile]
edges = terminals ++ honors

-- | List of bonus tiles.
bonuses :: [Tile]
bonuses = flowers ++ seasons

-- | List of red tiles.
reds :: [Tile]
reds = map mkBamboo [One, Five, Seven, Nine] ++ [mkDragon Red]

-- | List of green tiles.
greens :: [Tile]
greens = map mkBamboo [Two, Three, Four, Six, Eight] ++ [mkDragon Green]

-- | List of blue tiles.
blues :: [Tile]
blues = [mkCoin Eight] ++ winds ++ [mkDragon White]

-- | List of all regular tiles without bonus tiles.
regulars :: [Tile]
regulars = concat [coins, bamboos, characters, winds, dragons]

-- | List containing all tiles
allTiles :: [Tile]
allTiles = regulars ++ bonuses

