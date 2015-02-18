{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
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

-- | Data definition of tiles
--   along with tile related functions
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

instance MetaType Suit
instance MetaType Honor
instance MetaType Bonus

instance Chowable (Tile Suit)
instance Pungable (Tile Suit)
instance Pungable (Tile Honor)


-------------------------------------------------------------------------------

{- Constructors -}

mkWrap      :: Tile a -> WrapTile
mkWrap      = Wrap


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

