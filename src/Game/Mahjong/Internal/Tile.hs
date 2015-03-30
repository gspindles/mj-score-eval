-- |
-- Module      :  Game.Mahjong.Internal.Tile
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definitions and instances of tiles
--   along with tile aliases, collections
--   as well as predicates on tiles
--   and utility functions
module Game.Mahjong.Internal.Tile where

import Data.Maybe (fromJust)


-------------------------------------------------------------------------------

{- Data definition -}

-- | The tile types
data TileType = Coin | Bamboo | Character | Wind | Dragon | Flower | Season | Animal
                deriving (Bounded, Enum, Eq, Ord)

-- | The tile values from one to nine for the 3 suits
data Values   = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
                deriving (Bounded, Enum, Eq, Ord)

-- | The four winds
data Winds    = East | South | West | North
                deriving (Bounded, Enum, Eq, Ord)

-- | The three dragons
data Dragons  = Red | Green | White
                deriving (Bounded, Enum, Eq, Ord)

-- | The four flowers
data Flowers  = PlumBlossom | Orchid | Chrysanthemum | BambooTree
                deriving (Bounded, Enum, Eq, Ord)

-- | The four seasons
data Seasons  = Spring | Summer | Autumn | Winter
                deriving (Bounded, Enum, Eq, Ord)

-- | The four animals
data Animals  = Cat | Mouse | Cockerel | Centipede
                deriving (Bounded, Enum, Eq, Ord)


-- | The 8 kinds of tiles
data Tile     = CTile Values   -- ^ Coin Tile
              | BTile Values   -- ^ Bamboo Tile
              | KTile Values   -- ^ Character Tile
              | WTile Winds    -- ^ Wind Tile
              | DTile Dragons  -- ^ Dragon Tile
              | FTile Flowers  -- ^ Flower Tile
              | STile Seasons  -- ^ Season Tile
              | ATile Animals  -- ^ Animal Tile
                deriving (Eq, Ord)


-------------------------------------------------------------------------------

{- Class instances -}

instance Show TileType where
  show tt         = showHelper tt reps [Coin .. Animal]
    where
      reps :: [String]
      reps = ["C", "B", "K", "W", "D", "F", "S", "A"]

instance Show Values where
  show v          = show $ showHelper v [1..] [One .. Nine]

instance Show Winds where
  show w          = showHelper w reps [East .. North]
    where
      reps :: [String]
      reps = ["E", "S", "W", "N"]

instance Show Dragons where
  show d          = showHelper d reps [Red .. White]
    where
      reps :: [String]
      reps = ["R", "G", "W"]

instance Show Flowers where
  show f          = show $ showHelper f [1..] [PlumBlossom .. BambooTree]

instance Show Seasons where
  show s          = show $ showHelper s [1..] [Spring .. Winter]

instance Show Animals where
  show a          = show $ showHelper a [1..] [Cat .. Centipede]

instance Show Tile where
  show (CTile c)  = "C" ++ show c
  show (BTile b)  = "B" ++ show b
  show (KTile k)  = "K" ++ show k
  show (WTile w)  = "W" ++ show w
  show (DTile d)  = "D" ++ show d
  show (FTile f)  = "F" ++ show f
  show (STile s)  = "S" ++ show s
  show (ATile a)  = "A" ++ show a

showHelper :: (Eq a) => a -> [b] -> [a] -> b
showHelper a reps = fromJust . lookup a . flip zip reps


-------------------------------------------------------------------------------

{- Tile Aliases -}

c1, c2, c3, c4, c5, c6, c7, c8, c9 :: Tile
c1 = CTile One
c2 = CTile Two
c3 = CTile Three
c4 = CTile Four
c5 = CTile Five
c6 = CTile Six
c7 = CTile Seven
c8 = CTile Eight
c9 = CTile Nine

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: Tile
b1 = BTile One
b2 = BTile Two
b3 = BTile Three
b4 = BTile Four
b5 = BTile Five
b6 = BTile Six
b7 = BTile Seven
b8 = BTile Eight
b9 = BTile Nine

k1, k2, k3, k4, k5, k6, k7, k8, k9 :: Tile
k1 = KTile One
k2 = KTile Two
k3 = KTile Three
k4 = KTile Four
k5 = KTile Five
k6 = KTile Six
k7 = KTile Seven
k8 = KTile Eight
k9 = KTile Nine

w1, w2, w3, w4 :: Tile
w1 = WTile East
w2 = WTile South
w3 = WTile West
w4 = WTile North

d1, d2, d3 :: Tile
d1 = DTile Red
d2 = DTile Green
d3 = DTile White

f1, f2, f3, f4 :: Tile
f1 = FTile PlumBlossom
f2 = FTile Orchid
f3 = FTile Chrysanthemum
f4 = FTile BambooTree

s1, s2, s3, s4 :: Tile
s1 = STile Spring
s2 = STile Summer
s3 = STile Autumn
s4 = STile Winter

a1, a2, a3, a4 :: Tile
a1 = ATile Cat
a2 = ATile Mouse
a3 = ATile Cockerel
a4 = ATile Centipede


-------------------------------------------------------------------------------

{- Tile collections -}

coins, bamboos, characters, winds, dragons, flowers, seasons, animals :: [Tile]
coins      = map CTile [One         ..]
bamboos    = map BTile [One         ..]
characters = map KTile [One         ..]
winds      = map WTile [East        ..]
dragons    = map DTile [Red         ..]
flowers    = map FTile [PlumBlossom ..]
seasons    = map STile [Spring      ..]
animals    = map ATile [Cat         ..]

simples, terminals, suits, honors, edges, bonuses, extras :: [Tile]
simples    = concatMap (\x -> tail . init $ x)  [coins, bamboos, characters]
terminals  = concatMap (\x -> [head x, last x]) [coins, bamboos, characters]
suits      = coins     ++ bamboos ++ characters
honors     = winds     ++ dragons
edges      = terminals ++ honors
bonuses    = flowers   ++ seasons
extras     = bonuses   ++ animals

reds, greens, blues :: [Tile]
reds       = map BTile [One, Five, Seven, Nine]       ++ [DTile Red]
greens     = map BTile [Two, Three, Four, Six, Eight] ++ [DTile Green]
blues      =           [CTile Eight] ++ winds         ++ [DTile White]

regulars, allTiles :: [Tile]
regulars   = coins ++ bamboos ++ characters ++ winds ++ dragons
allTiles   = regulars ++ bonuses


-------------------------------------------------------------------------------

{- Utility functions -}

tileType :: Tile -> TileType
tileType (CTile _) = Coin
tileType (BTile _) = Bamboo
tileType (KTile _) = Character
tileType (WTile _) = Wind
tileType (DTile _) = Dragon
tileType (FTile _) = Flower
tileType (STile _) = Season
tileType (ATile _) = Animal


-------------------------------------------------------------------------------

{- Dora -}

class Dora a where
  dora                  :: a -> a
  reverseDora           :: a -> a

instance Dora Values where
  dora                  = doraHelper
  reverseDora           = rDoraHelper

instance Dora Winds where
  dora                  = doraHelper
  reverseDora           = rDoraHelper

instance Dora Dragons where
  dora                  = doraHelper
  reverseDora           = rDoraHelper

instance Dora Flowers where
  dora                  = doraHelper
  reverseDora           = rDoraHelper

instance Dora Seasons where
  dora                  = doraHelper
  reverseDora           = rDoraHelper

instance Dora Animals where
  dora                  = doraHelper
  reverseDora           = rDoraHelper

instance Dora Tile where
  dora        (CTile c) = CTile $ dora c
  dora        (BTile b) = BTile $ dora b
  dora        (KTile k) = KTile $ dora k
  dora        (WTile w) = WTile $ dora w
  dora        (DTile d) = DTile $ dora d
  dora        (FTile f) = FTile $ dora f
  dora        (STile s) = STile $ dora s
  dora        (ATile a) = ATile $ dora a

  reverseDora (CTile c) = CTile $ reverseDora c
  reverseDora (BTile b) = BTile $ reverseDora b
  reverseDora (KTile k) = KTile $ reverseDora k
  reverseDora (WTile w) = WTile $ reverseDora w
  reverseDora (DTile d) = DTile $ reverseDora d
  reverseDora (FTile f) = FTile $ reverseDora f
  reverseDora (STile s) = STile $ reverseDora s
  reverseDora (ATile a) = ATile $ reverseDora a

doraHelper  :: (Bounded a, Enum a, Eq a) => a -> a
doraHelper a            = if a == maxBound then minBound else succ a

rDoraHelper :: (Bounded a, Enum a, Eq a) => a -> a
rDoraHelper a           = if a == minBound then maxBound else pred a

