{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

-- |
-- Module      :  Game.Mahjong.Experimental.Tile
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
module Game.Mahjong.Experimental.Tile where

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

-- | Meta tile types
data Suit
data Honor
data Bonus

-- | Meta tile classes
class Chowable a
class Pungable a

-- | The 8 kinds of tiles 
data Tile a where
  CTile :: Values  -> Tile Suit
  BTile :: Values  -> Tile Suit
  KTile :: Values  -> Tile Suit
  WTile :: Winds   -> Tile Honor
  DTile :: Dragons -> Tile Honor
  FTile :: Flowers -> Tile Bonus
  STile :: Seasons -> Tile Bonus
  ATile :: Animals -> Tile Bonus

-- | A wrapper for tile to obtain heterogeneous list
data WrapTile where
  Wrap :: Tile a -> WrapTile

type Tiles = [WrapTile]


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

instance Show (Tile a) where
  show (CTile c)  = "C" ++ show c
  show (BTile b)  = "B" ++ show b
  show (KTile k)  = "K" ++ show k
  show (WTile w)  = "W" ++ show w
  show (DTile d)  = "D" ++ show d
  show (FTile f)  = "F" ++ show f
  show (STile s)  = "S" ++ show s
  show (ATile a)  = "A" ++ show a

instance Show WrapTile where
  show (Wrap t)   = show t

showHelper :: (Eq a) => a -> [b] -> [a] -> b
showHelper a reps = fromJust . lookup a . flip zip reps


instance Eq (Tile a) where
  (==)                         = eqHelper

instance Eq (WrapTile) where
  w1 == w2                     = liftWrap2 eqHelper w1 w2

eqHelper :: Tile a -> Tile b -> Bool
eqHelper (CTile c1) (CTile c2) = c1 == c2
eqHelper (BTile b1) (BTile b2) = b1 == b2
eqHelper (KTile k1) (KTile k2) = k1 == k2
eqHelper (WTile w1) (WTile w2) = w1 == w2
eqHelper (DTile d1) (DTile d2) = d1 == d2
eqHelper (FTile f1) (FTile f2) = f1 == f2
eqHelper (STile s1) (STile s2) = s1 == s2
eqHelper (ATile a1) (ATile a2) = a1 == a2
eqHelper _          _          = False


instance Ord (Tile a) where
  compare          = ordHelper

instance Ord WrapTile where
  compare w1 w2    = liftWrap2 ordHelper w1 w2

ordHelper :: Tile a -> Tile b -> Ordering
ordHelper t1 t2    = compare (tileRank t1) (tileRank t2)

tileRank :: Tile a -> Int
tileRank (CTile c) =  0 + showHelper c [1..] [One .. Nine]
tileRank (BTile b) = 10 + showHelper b [1..] [One .. Nine]
tileRank (KTile k) = 20 + showHelper k [1..] [One .. Nine]
tileRank (WTile w) = 30 + showHelper w [1..] [East .. North]
tileRank (DTile d) = 40 + showHelper d [1..] [Red .. White]
tileRank (FTile f) = 50 + showHelper f [1..] [PlumBlossom .. BambooTree]
tileRank (STile s) = 60 + showHelper s [1..] [Spring .. Winter]
tileRank (ATile a) = 70 + showHelper a [1..] [Cat .. Centipede]


instance Chowable Suit
instance Pungable Suit
instance Pungable Honor


-------------------------------------------------------------------------------

{- Constructors -}

mkWrap      :: Tile a -> WrapTile
mkWrap      = Wrap


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


-------------------------------------------------------------------------------

{- Utility functions -}

tileType :: Tile a -> TileType
tileType (CTile _) = Coin
tileType (BTile _) = Bamboo
tileType (KTile _) = Character
tileType (WTile _) = Wind
tileType (DTile _) = Dragon
tileType (FTile _) = Flower
tileType (STile _) = Season
tileType (ATile _) = Animal

liftWrap :: (forall a. Tile a -> b) -> WrapTile -> b
liftWrap f (Wrap t) = f t

liftWrapT :: (forall a. Tile a -> Tile a) -> WrapTile -> WrapTile
liftWrapT f (Wrap t) = Wrap $ f t

liftWrap2 :: (forall a b. Tile a -> Tile b -> c) -> WrapTile -> WrapTile -> c
liftWrap2 f (Wrap t1) (Wrap t2) = f t1 t2

mapWrap :: (forall a. Tile a -> b) -> Tiles -> [b]
mapWrap f ws = map (liftWrap f) ws

mapWrapT :: (forall a. Tile a -> Tile a) -> Tiles -> Tiles
mapWrapT f ws = map (liftWrapT f) ws


-------------------------------------------------------------------------------

{- Dora -}

class Dora a where
  dora        :: a -> a
  reverseDora :: a -> a

instance Dora (Tile t) where
  dora (CTile c)        = if c == Nine       then CTile One         else CTile $ succ c
  dora (BTile b)        = if b == Nine       then BTile One         else BTile $ succ b
  dora (KTile k)        = if k == Nine       then KTile One         else KTile $ succ k
  dora (WTile w)        = if w == North      then WTile East        else WTile $ succ w
  dora (DTile d)        = if d == White      then DTile Red         else DTile $ succ d
  dora (FTile f)        = if f == BambooTree then FTile PlumBlossom else FTile $ succ f
  dora (STile s)        = if s == Winter     then STile Spring      else STile $ succ s
  dora (ATile a)        = if a == Centipede  then ATile Cat         else ATile $ succ a

  reverseDora (CTile c) = if c == One         then CTile Nine       else CTile $ pred c
  reverseDora (BTile b) = if b == One         then BTile Nine       else BTile $ pred b
  reverseDora (KTile k) = if k == One         then KTile Nine       else KTile $ pred k
  reverseDora (WTile w) = if w == East        then WTile North      else WTile $ pred w
  reverseDora (DTile d) = if d == Red         then DTile White      else DTile $ pred d
  reverseDora (FTile f) = if f == PlumBlossom then FTile BambooTree else FTile $ pred f
  reverseDora (STile s) = if s == Spring      then STile Winter     else STile $ pred s
  reverseDora (ATile a) = if a == Cat         then ATile Centipede  else ATile $ pred a

instance Dora WrapTile where
  dora        = liftWrapT dora
  reverseDora = liftWrapT reverseDora

