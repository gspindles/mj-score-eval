{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

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

-- | Meta tile types
data Suit
data Honor
data Bonus

class MetaType a
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
  show Coin            = "C"
  show Bamboo          = "B"
  show Character       = "K"
  show Wind            = "W"
  show Dragon          = "D"
  show Flower          = "F"
  show Season          = "S"
  show Animal          = "A"

instance Show Values where
  show v = show $ showHelper v [One .. Nine]

showHelper :: (Eq a) => a -> [a] -> Int
showHelper a = fromJust . lookup a . flip zip [1..]

instance Show Winds where
  show (East)          = "E"
  show (South)         = "S"
  show (West)          = "W"
  show (North)         = "N"

instance Show Dragons where
  show (Red)           = "R"
  show (Green)         = "G"
  show (White)         = "W"

instance Show Flowers where
  show (PlumBlossom)   = "1"
  show (Orchid)        = "2"
  show (Chrysanthemum) = "3"
  show (BambooTree)    = "4"

instance Show Seasons where
  show (Spring)        = "1"
  show (Summer)        = "2"
  show (Autumn)        = "3"
  show (Winter)        = "4"

instance Show Animals where
  show (Cat)           = "1"
  show (Mouse)         = "2"
  show (Cockerel)      = "3"
  show (Centipede)     = "4"

instance Show (Tile a) where
  show (CTile c)       = "C" ++ show c
  show (BTile b)       = "B" ++ show b
  show (KTile k)       = "K" ++ show k
  show (WTile w)       = "W" ++ show w
  show (DTile d)       = "D" ++ show d
  show (FTile f)       = "F" ++ show f
  show (STile s)       = "S" ++ show s
  show (ATile a)       = "A" ++ show a

instance Show WrapTile where
  show (Wrap t) = show t

instance Eq (Tile a) where
  (==) = eqHelper

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

instance Eq (WrapTile) where
  w1 == w2 = liftWrap2 eqHelper w1 w2

instance Ord (Tile a) where
  compare = ordHelper

ordHelper :: Tile a -> Tile b -> Ordering
ordHelper t1 t2 = compare (tileRank t1) (tileRank t2)

tileRank :: Tile a -> Int
tileRank (CTile c) = 10 + showHelper c [One .. Nine]
tileRank (BTile b) = 20 + showHelper b [One .. Nine]
tileRank (KTile k) = 30 + showHelper k [One .. Nine]
tileRank (WTile w) = 40 + showHelper w [East .. North]
tileRank (DTile d) = 50 + showHelper d [Red .. White]
tileRank (FTile f) = 60 + showHelper f [PlumBlossom .. BambooTree]
tileRank (STile s) = 70 + showHelper s [Spring .. Winter]
tileRank (ATile a) = 80 + showHelper a [Cat .. Centipede]

instance Ord WrapTile where
  compare w1 w2 = liftWrap2 ordHelper w1 w2

instance MetaType Suit
instance MetaType Honor
instance MetaType Bonus

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

{- Predicates for determining tile types -}

tileType :: Tile a -> TileType
tileType (CTile _) = Coin
tileType (BTile _) = Bamboo
tileType (KTile _) = Character
tileType (WTile _) = Wind
tileType (DTile _) = Dragon
tileType (FTile _) = Flower
tileType (STile _) = Season
tileType (ATile _) = Animal

isCoinT, isBambooT, isCharacterT, isWindT, isDragonT, isFlowerT, isSeasonT, isAnimalT :: Tile a -> Bool
isCoinT                = (==) Coin . tileType
isBambooT              = (==) Bamboo . tileType 
isCharacterT           = (==) Character . tileType 
isWindT                = (==) Wind . tileType
isDragonT              = (==) Dragon . tileType
isFlowerT              = (==) Flower . tileType
isSeasonT              = (==) Season . tileType
isAnimalT              = (==) Animal . tileType

isSimpleT, isTerminalT, isSuitT, isHonorT, isEdgeT, isBonusT :: Tile a -> Bool
isSimpleT              = and . zipWith id [isSuitT, not . isTerminalT] . repeat
isTerminalT (CTile v)  = elem v [One, Nine]
isTerminalT (BTile v)  = elem v [One, Nine]
isTerminalT (KTile v)  = elem v [One, Nine]
isTerminalT _          = False
isSuitT                = or . zipWith id [isCoinT, isBambooT, isCharacterT] . repeat
isHonorT               = or . zipWith id [isWindT, isDragonT] . repeat 
isEdgeT                = or . zipWith id [isTerminalT, isHonorT] . repeat
isBonusT               = or . zipWith id [isFlowerT, isSeasonT, isAnimalT] . repeat

isRedT, isGreenT, isBlueT :: Tile a -> Bool
isRedT                 = flip elem reds . Wrap
isGreenT               = flip elem greens . Wrap
isBlueT                = flip elem blues . Wrap


-------------------------------------------------------------------------------

{- Utility functions -}

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

dora :: Tile a -> Tile a
dora (CTile c) = if c == Nine       then CTile One         else CTile $ succ c
dora (BTile b) = if b == Nine       then BTile One         else BTile $ succ b
dora (KTile k) = if k == Nine       then KTile One         else KTile $ succ k
dora (WTile w) = if w == North      then WTile East        else WTile $ succ w
dora (DTile d) = if d == White      then DTile Red         else DTile $ succ d
dora (FTile f) = if f == BambooTree then FTile PlumBlossom else FTile $ succ f
dora (STile s) = if s == Winter     then STile Spring      else STile $ succ s
dora (ATile a) = if a == Centipede  then ATile Cat         else ATile $ succ a

reverseDora :: Tile a -> Tile a
reverseDora (CTile c) = if c == One         then CTile Nine       else CTile $ pred c
reverseDora (BTile b) = if b == One         then BTile Nine       else BTile $ pred b
reverseDora (KTile k) = if k == One         then KTile Nine       else KTile $ pred k
reverseDora (WTile w) = if w == East        then WTile North      else WTile $ pred w
reverseDora (DTile d) = if d == Red         then DTile White      else DTile $ pred d
reverseDora (FTile f) = if f == PlumBlossom then FTile BambooTree else FTile $ pred f
reverseDora (STile s) = if s == Spring      then STile Winter     else STile $ pred s
reverseDora (ATile a) = if a == Cat         then ATile Centipede  else ATile $ pred a

