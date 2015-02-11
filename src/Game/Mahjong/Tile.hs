{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

-- |
-- Module      :  Game.Mahjong.Tile
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definition of tiles
--   along with tile related functions
module Game.Mahjong.Tile
  ( -- Tile data
    TileType, Values, Winds, Dragons, Flowers, Seasons, Animals
  , Tile, WrapTile
  , Suit, Honor, Bonus

    -- constructors :: (MetaType a) => Tile a -> WrapTile
  , mkWrap

    -- Tiles :: (MetaType a) => Tile a
  , c1, c2, c3, c4, c5, c6, c7, c8, c9
  , b1, b2, b3, b4, b5, b6, b7, b8, b9
  , k1, k2, k3, k4, k5, k6, k7, k8, k9
  , w1, w2, w3, w4
  , d1, d2, d3
  , f1, f2, f3, f4
  , s1, s2, s3, s4

    -- Tile collections :: [WrapTile]
  , coins, bamboos, characters, winds, dragons, flowers, seasons, animals
  , simples, terminals, suits, honors, edges, bonuses, extras
  , greens, reds, blues
  , regulars, allTiles

    -- Tile predicates :: Tile a -> Bool
  , isCoin, isBamboo, isCharacter, isWind, isDragon, isFlower, isSeason, isAnimal
  , isSimple, isTerminal, isSuit, isHonor, isEdge, isBonus
  , isGreen, isRed, isBlue

    -- Utility functions
  , getType
  , liftWrap, liftWrapT, liftWrap2, mapWrap, mapWrapT
  , mjSet, getWall, impureWall
  , dora, reverseDora
) where

import Data.Map (Map, insert, (!), elems, singleton)
import Data.Maybe (fromJust)
import System.IO.Unsafe
import System.Random


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
class MetaType a

-- | Suit, Honor, and Bonus are used as phantoms for Tile
data Suit
data Honor
data Bonus

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
  show v = show . fromJust . lookup v $ flip zip [1..9] [One .. Nine]

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
  show (CTile v)       = "C" ++ show v
  show (BTile v)       = "B" ++ show v
  show (KTile v)       = "K" ++ show v
  show (WTile v)       = "W" ++ show v
  show (DTile v)       = "D" ++ show v
  show (FTile v)       = "F" ++ show v
  show (STile v)       = "S" ++ show v
  show (ATile v)       = "A" ++ show v

instance Show WrapTile where
  show (Wrap t) = show t

instance Eq (Tile a) where
  (==) = heq

instance Eq (WrapTile) where
  w1 == w2 = liftWrap2 heq w1 w2

instance MetaType Suit
instance MetaType Honor
instance MetaType Bonus


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

coins      :: [WrapTile]
coins      = map (Wrap . CTile) [One ..]

bamboos    :: [WrapTile]
bamboos    = map (Wrap . BTile) [One ..]

characters :: [WrapTile]
characters = map (Wrap . KTile) [One ..]

winds      :: [WrapTile]
winds      = map (Wrap . WTile) [East ..]

dragons    :: [WrapTile]
dragons    = map (Wrap . DTile) [Red ..]

flowers    :: [WrapTile]
flowers    = map (Wrap . FTile) [PlumBlossom ..]

seasons    :: [WrapTile]
seasons    = map (Wrap . STile) [Spring ..]

animals    :: [WrapTile]
animals    = map (Wrap . ATile) [Cat ..]

simples    :: [WrapTile]
simples    = concatMap (\x -> tail . init $ x) [coins, bamboos, characters]

terminals  :: [WrapTile]
terminals  = concatMap (\x -> [head x, last x]) [coins, bamboos, characters]

suits      :: [WrapTile]
suits      = coins ++ bamboos ++ characters

honors     :: [WrapTile]
honors     = winds ++ dragons

edges      :: [WrapTile]
edges      = terminals ++ honors

bonuses    :: [WrapTile]
bonuses    = flowers ++ seasons

extras     :: [WrapTile]
extras     = bonuses ++ animals

greens     :: [WrapTile]
greens     = map (Wrap . BTile) [Two, Three, Four, Six, Eight]
             ++ [Wrap $ DTile Green]

reds       :: [WrapTile]
reds       = map (Wrap . BTile) [One, Five, Seven, Nine]
             ++ [Wrap $ DTile Red]

blues      :: [WrapTile]
blues      = [Wrap $ CTile Eight] ++ winds ++ [Wrap $ DTile White]

regulars   :: [WrapTile]
regulars   = coins ++ bamboos ++ characters ++ winds ++ dragons

allTiles   :: [WrapTile]
allTiles   = regulars ++ bonuses


-------------------------------------------------------------------------------

{- Predicates for determining tile types -}

isCoin :: Tile a -> Bool
isCoin (CTile _)      = True
isCoin _              = False

isBamboo :: Tile a -> Bool
isBamboo (BTile _)    = True
isBamboo _            = False

isCharacter :: Tile a -> Bool
isCharacter (KTile _) = True
isCharacter _         = False

isWind :: Tile a -> Bool
isWind (WTile _)      = True
isWind _              = False

isDragon :: Tile a -> Bool
isDragon (DTile _)    = True
isDragon _            = False

isFlower :: Tile a -> Bool
isFlower (FTile _)    = True
isFlower _            = False

isSeason :: Tile a -> Bool
isSeason (STile _)    = True
isSeason _            = False

isAnimal :: Tile a -> Bool
isAnimal (ATile _)    = True
isAnimal _            = False

isSimple :: Tile a -> Bool
isSimple (CTile v)    = elem v [Two .. Eight]
isSimple (BTile v)    = elem v [Two .. Eight]
isSimple (KTile v)    = elem v [Two .. Eight]
isSimple _            = False

isTerminal :: Tile a -> Bool
isTerminal (CTile v)  = elem v [One, Nine]
isTerminal (BTile v)  = elem v [One, Nine]
isTerminal (KTile v)  = elem v [One, Nine]
isTerminal _          = False

isSuit :: Tile a -> Bool
isSuit  = or . zipWith id [isCoin, isBamboo, isCharacter] . repeat

isHonor :: Tile a -> Bool
isHonor = or . zipWith id [isWind, isDragon] . repeat 

isEdge :: Tile a -> Bool
isEdge  = or . zipWith id [isTerminal, isHonor] . repeat

isBonus :: Tile a -> Bool
isBonus = or . zipWith id [isFlower, isSeason, isAnimal] . repeat

isGreen :: Tile a -> Bool
isGreen = flip elem greens . Wrap

isRed :: Tile a -> Bool
isRed   = flip elem reds . Wrap

isBlue :: Tile a -> Bool
isBlue  = flip elem blues . Wrap


-------------------------------------------------------------------------------

{- Utility functions -}

getType :: Tile a -> TileType
getType (CTile _) = Coin
getType (BTile _) = Bamboo
getType (KTile _) = Character
getType (WTile _) = Wind
getType (DTile _) = Dragon
getType (FTile _) = Flower
getType (STile _) = Season
getType (ATile _) = Animal

heq :: Tile a -> Tile b -> Bool
heq (CTile v1) (CTile v2) = v1 == v2
heq (BTile v1) (BTile v2) = v1 == v2
heq (KTile v1) (KTile v2) = v1 == v2
heq (WTile w1) (WTile w2) = w1 == w2
heq (DTile d1) (DTile d2) = d1 == d2
heq (FTile f1) (FTile f2) = f1 == f2
heq (STile s1) (STile s2) = s1 == s2
heq (ATile a1) (ATile a2) = a1 == a2
heq _          _          = False

liftWrap :: (forall a. Tile a -> b) -> WrapTile -> b
liftWrap f (Wrap t) = f t

liftWrapT :: (forall a. Tile a -> Tile a) -> WrapTile -> WrapTile
liftWrapT f (Wrap t) = Wrap $ f t

liftWrap2 :: (forall a b. Tile a -> Tile b -> c) -> WrapTile -> WrapTile -> c
liftWrap2 f (Wrap t1) (Wrap t2) = f t1 t2

mapWrap :: (forall a. Tile a -> b) -> [WrapTile] -> [b]
mapWrap f ws = map (liftWrap f) ws

mapWrapT :: (forall a. Tile a -> Tile a) -> [WrapTile] -> [WrapTile]
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


-------------------------------------------------------------------------------

{- Wall building -}

mjSet :: [WrapTile]
mjSet      = (concatMap (take 4 . repeat) $ regulars) ++ bonuses

getWall :: Int -> [WrapTile]
getWall a  = fst $ fisherYates (mkStdGen a) mjSet

impureWall :: IO [WrapTile]
impureWall = do
  r <- randNumber
  return $ getWall r

randNumber :: IO Int
randNumber = return $ mod (unsafePerformIO randomIO) 144

-- | Fisher Yates Algorithm
-- | Source:  http://www.haskell.org/haskellwiki/Random_shuffle
fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l  =
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate       = zip [1..]
    initial x gen  = (singleton 0 x, gen)
