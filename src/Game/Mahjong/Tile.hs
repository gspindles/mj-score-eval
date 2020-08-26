{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

-- | Tile module contains data definitions and class instances for tile.
--   Additionally, this package also exports tile names, collections,
--   some utility functions, as well as wall building.
module Game.Mahjong.Tile (
  -- * Tile & Tile type
  TileType(..),
  Tile(..),
  Values(..), Winds(..), Dragons(..), Flowers(..), Seasons(..),

  -- ** Utility functions
  tileType, tileValue, isSameTileType,

  -- ** Construction
  mkCoin, mkBamboo, mkCharacter, mkWind, mkDragon, mkFlower, mkSeason
) where

import Game.Mahjong.Class

import Data.Kind (Type)
import Data.List (nub)
import Data.Maybe (fromJust)


-------------------------------------------------------------------------------
-- Data definition
-------------------------------------------------------------------------------

-- | The tile types.
data TileType
  = Coin
  | Bamboo
  | Character
  | Wind
  | Dragon
  | Flower
  | Season
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | The tile values from one to nine for the 3 suits.
data Values
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | The four wind value.
data Winds
  = East
  | South
  | West
  | North
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | The three dragon values.
data Dragons
  = Red
  | Green
  | White
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | The four flowers.
data Flowers
  = PlumBlossom
  | Orchid
  | Chrysanthemum
  | BambooTree
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | The four seasons.
data Seasons
  = Spring
  | Summer
  | Autumn
  | Winter
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | pi quantify the tile type
data family Sing :: k -> Type
data instance Sing (tt :: TileType) where
  SC :: Sing 'Coin
  SB :: Sing 'Bamboo
  SK :: Sing 'Character
  SW :: Sing 'Wind
  SD :: Sing 'Dragon
  SF :: Sing 'Flower
  SS :: Sing 'Season

-- | map the tile type to their value type
type family TileValue (tt :: TileType) where
  TileValue 'Coin      = Values
  TileValue 'Bamboo    = Values
  TileValue 'Character = Values
  TileValue 'Wind      = Winds
  TileValue 'Dragon    = Dragons
  TileValue 'Flower    = Flowers
  TileValue 'Season    = Seasons

type Ctxt tt = (
    Bounded (TileValue tt)
  , Enum (TileValue tt)
  , Eq (TileValue tt)
  , Ord (TileValue tt)
  , Pretty (TileValue tt)
  , Show (TileValue tt)
  , Cycle (TileValue tt)
  )

-- | sigma tile definition
data Tile where
  Tile :: Ctxt tt => Sing (tt :: TileType) -> TileValue tt -> Tile


-------------------------------------------------------------------------------
-- Typeclass instances
-------------------------------------------------------------------------------

instance Show (Sing (tt :: TileType)) where
  show st = show $ demote st

deriving instance Show Tile


instance Eq (Sing (tt :: TileType)) where
  st1 == st2 = singEq st1 st2

singEq :: Sing (t1 :: TileType) -> Sing (t2 :: TileType) -> Bool
singEq SC SC = True
singEq SB SB = True
singEq SK SK = True
singEq SW SW = True
singEq SD SD = True
singEq SF SF = True
singEq SS SS = True
singEq _  _  = False

instance Eq Tile where
  (Tile SC v1) == (Tile SC v2) = v1 == v2
  (Tile SB v1) == (Tile SB v2) = v1 == v2
  (Tile SK v1) == (Tile SK v2) = v1 == v2
  (Tile SW v1) == (Tile SW v2) = v1 == v2
  (Tile SD v1) == (Tile SD v2) = v1 == v2
  (Tile SF v1) == (Tile SF v2) = v1 == v2
  (Tile SS v1) == (Tile SS v2) = v1 == v2
  (Tile _  _ ) == (Tile _  _ ) = False


instance Ord (Sing (tt :: TileType)) where
  compare st1 st2 = singOrd st1 st2

singOrd :: Sing (t1 :: TileType) -> Sing (t2 :: TileType) -> Ordering
singOrd st1 st2 = compare (demote st1) (demote st2)

instance Ord Tile where
  compare (Tile SC v1) (Tile SC v2) = compare v1 v2
  compare (Tile SB v1) (Tile SB v2) = compare v1 v2
  compare (Tile SK v1) (Tile SK v2) = compare v1 v2
  compare (Tile SW v1) (Tile SW v2) = compare v1 v2
  compare (Tile SD v1) (Tile SD v2) = compare v1 v2
  compare (Tile SF v1) (Tile SF v2) = compare v1 v2
  compare (Tile SS v1) (Tile SS v2) = compare v1 v2
  compare (Tile t1 _ ) (Tile t2 _ ) = singOrd t1 t2


instance Pretty TileType where
  pp tt    = ppHelper tt reps [Coin .. Season]
    where
      reps :: [String]
      reps = ["C", "B", "K", "W", "D", "F", "S"]

instance Pretty Values where
  pp v = pp $ ppHelper v ([1..] :: [Int]) [One .. Nine]

instance Pretty Winds where
  pp w     = ppHelper w reps [East .. North]
    where
      reps :: [String]
      reps = ["E", "S", "W", "N"]

instance Pretty Dragons where
  pp d     = ppHelper d reps [Red .. White]
    where
      reps :: [String]
      reps = ["R", "G", "W"]

instance Pretty Flowers where
  pp f = pp $ ppHelper f infInts [PlumBlossom .. BambooTree]

instance Pretty Seasons where
  pp s = pp $ ppHelper s infInts [Spring .. Winter]

infInts :: [Int]
infInts = [1..]

instance Pretty (Sing (tt :: TileType)) where
  pp st = pp $ demote st

instance Pretty Tile where
  pp (Tile tt tv)  = pp tt ++ pp tv

ppHelper :: (Eq a) => a -> [b] -> [a] -> b
ppHelper a reps = fromJust . lookup a . flip zip reps


instance TilePred Tile where
  isCoin                = (==) Coin      . tileType
  isBamboo              = (==) Bamboo    . tileType
  isCharacter           = (==) Character . tileType

  isSimple              = allCond [isSuit, not . isTerminal]
  isTerminal (Tile t v) = case t of
                            SC -> elem v [One, Nine]
                            SB -> elem v [One, Nine]
                            SK -> elem v [One, Nine]
                            _  -> False
  isSuit                = anyCond [isCoin, isBamboo, isCharacter]

  isWind                = (==) Wind   . tileType
  isDragon              = (==) Dragon . tileType

  isHonor               = anyCond [isWind, isDragon]
  isEdge                = anyCond [isTerminal, isHonor]

  isFlower              = (==) Flower . tileType
  isSeason              = (==) Season . tileType

  isBonus               = anyCond [isFlower, isSeason]

  isGreen               = flip elem greens
    where
      greens :: [Tile]
      greens = map (Tile SB) [Two, Three, Four, Six, Eight] ++ [Tile SD Green]

  isRed                 = flip elem reds
    where
      reds :: [Tile]
      reds = map (Tile SB) [One, Five, Seven, Nine] ++ [Tile SD Red]

instance Cycle Values where
  next = nextHelper
  prev = prevHelper

instance Cycle Winds where
  next = nextHelper
  prev = prevHelper

instance Cycle Dragons where
  next = nextHelper
  prev = prevHelper

instance Cycle Flowers where
  next = nextHelper
  prev = prevHelper

instance Cycle Seasons where
  next = nextHelper
  prev = prevHelper

instance Cycle Tile where
  next (Tile t v) = Tile t $ next v
  prev (Tile t v) = Tile t $ prev v


-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

-- | demote singleton to value
demote :: Sing (tt :: TileType) -> TileType
demote SC = Coin
demote SB = Bamboo
demote SK = Character
demote SW = Wind
demote SD = Dragon
demote SF = Flower
demote SS = Season

-- | Gets the type of tile.
tileType :: Tile -> TileType
tileType (Tile tt _) = demote tt

-- | Gets the numeric value of the tile.
tileValue :: Tile -> Int
tileValue (Tile _ tv) = fromEnum tv + 1

-- | Check if the list of tiles have the same tile type
isSameTileType :: [Tile] -> Bool
isSameTileType = (==) 1 . length . nub . fmap tileType


-------------------------------------------------------------------------------
-- Contruction
-------------------------------------------------------------------------------

mkCoin, mkBamboo, mkCharacter :: Values -> Tile
mkCoin      = Tile SC
mkBamboo    = Tile SB
mkCharacter = Tile SK

mkWind :: Winds -> Tile
mkWind      = Tile SW

mkDragon :: Dragons -> Tile
mkDragon    = Tile SD

mkFlower :: Flowers -> Tile
mkFlower    = Tile SF

mkSeason :: Seasons -> Tile
mkSeason    = Tile SS

