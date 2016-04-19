{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

-- |
-- Module      :  Game.Mahjong.Tile
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Tile Module contains data definitions and class instances for tile.
--   Additionally, this package also exports tile names, collections,
--   some utility functions, as well as wall building.
module Game.Mahjong.Tile (
  -- * Tile & Tile type
  TileType(..),
  Tile,

  -- ** Individual tiles
  c1, c2, c3, c4, c5, c6, c7, c8, c9,
  b1, b2, b3, b4, b5, b6, b7, b8, b9,
  k1, k2, k3, k4, k5, k6, k7, k8, k9,
  wn, ws, we, ww,
  dr, dg, dw,
  f1, f2, f3, f4,
  s1, s2, s3, s4,
  a1, a2, a3, a4,

  -- ** Tile collections
  coins, bamboos, characters, winds, dragons, flowers, seasons, animals,
  simples, terminals, suits, honors, edges, bonuses, extras,
  reds, greens, blues,
  regulars, allTiles,

  -- ** Utility functions
  tileType, tileValue, isEightOrNine, isSameTileType,

  -- ** Wall building
  mjSet, getWall, randomWall
) where

import Game.Mahjong.Class

import Data.List (nub, sort)
import Data.Map (Map, insert, (!), elems, singleton)
import Data.Maybe (fromJust)
import Data.Singletons (Sing)
import System.Random (randomR, randomIO, mkStdGen, RandomGen)


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
  | Animal
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

-- | The four animals.
data Animals
  = Cat
  | Mouse
  | Cockerel
  | Centipede
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | pi quantify the tile type
data instance Sing (tt :: TileType) where
  SC :: Sing Coin
  SB :: Sing Bamboo
  SK :: Sing Character
  SW :: Sing Wind
  SD :: Sing Dragon
  SF :: Sing Flower
  SS :: Sing Season
  SA :: Sing Animal

-- | map the tile type to their value type
type family TileValue (tt :: TileType) where
  TileValue Coin      = Values
  TileValue Bamboo    = Values
  TileValue Character = Values
  TileValue Wind      = Winds
  TileValue Dragon    = Dragons
  TileValue Flower    = Flowers
  TileValue Season    = Seasons
  TileValue Animal    = Animals

type Ctxt tt = (
    Bounded (TileValue tt)
  , Enum (TileValue tt)
  , Eq (TileValue tt)
  , Ord (TileValue tt)
  , Pretty (TileValue tt)
  , Show (TileValue tt)
  , Loop (TileValue tt)
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
singEq SA SA = True
singEq _  _  = False

instance Eq Tile where
  (Tile SC v1) == (Tile SC v2) = v1 == v2
  (Tile SB v1) == (Tile SB v2) = v1 == v2
  (Tile SK v1) == (Tile SK v2) = v1 == v2
  (Tile SW v1) == (Tile SW v2) = v1 == v2
  (Tile SD v1) == (Tile SD v2) = v1 == v2
  (Tile SF v1) == (Tile SF v2) = v1 == v2
  (Tile SS v1) == (Tile SS v2) = v1 == v2
  (Tile SA v1) == (Tile SA v2) = v1 == v2
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
  compare (Tile SA v1) (Tile SA v2) = compare v1 v2
  compare (Tile t1 _ ) (Tile t2 _ ) = singOrd t1 t2


instance Pretty TileType where
  pp tt    = ppHelper tt reps [Coin .. Animal]
    where
      reps :: [String]
      reps = ["C", "B", "K", "W", "D", "F", "S", "A"]

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

instance Pretty Animals where
  pp a = pp $ ppHelper a infInts [Cat .. Centipede]

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
  isAnimal              = (==) Animal . tileType

  isBonus               = anyCond [isFlower, isSeason, isAnimal]

  isRed                 = flip elem reds
  isGreen               = flip elem greens
  isBlue                = flip elem blues


instance Loop Values where
  next = nextHelper
  prev = prevHelper

instance Loop Winds where
  next = nextHelper
  prev = prevHelper

instance Loop Dragons where
  next = nextHelper
  prev = prevHelper

instance Loop Flowers where
  next = nextHelper
  prev = prevHelper

instance Loop Seasons where
  next = nextHelper
  prev = prevHelper

instance Loop Animals where
  next = nextHelper
  prev = prevHelper

instance Loop Tile where
  next (Tile t v) = Tile t $ next v
  prev (Tile t v) = Tile t $ prev v


-------------------------------------------------------------------------------
-- Tile Aliases
-------------------------------------------------------------------------------

c1, c2, c3, c4, c5, c6, c7, c8, c9 :: Tile
c1 = Tile SC One
c2 = Tile SC Two
c3 = Tile SC Three
c4 = Tile SC Four
c5 = Tile SC Five
c6 = Tile SC Six
c7 = Tile SC Seven
c8 = Tile SC Eight
c9 = Tile SC Nine

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: Tile
b1 = Tile SB One
b2 = Tile SB Two
b3 = Tile SB Three
b4 = Tile SB Four
b5 = Tile SB Five
b6 = Tile SB Six
b7 = Tile SB Seven
b8 = Tile SB Eight
b9 = Tile SB Nine

k1, k2, k3, k4, k5, k6, k7, k8, k9 :: Tile
k1 = Tile SK One
k2 = Tile SK Two
k3 = Tile SK Three
k4 = Tile SK Four
k5 = Tile SK Five
k6 = Tile SK Six
k7 = Tile SK Seven
k8 = Tile SK Eight
k9 = Tile SK Nine

we, ws, ww, wn :: Tile
we = Tile SW East
ws = Tile SW South
ww = Tile SW West
wn = Tile SW North

dr, dg, dw :: Tile
dr = Tile SD Red
dg = Tile SD Green
dw = Tile SD White

f1, f2, f3, f4 :: Tile
f1 = Tile SF PlumBlossom
f2 = Tile SF Orchid
f3 = Tile SF Chrysanthemum
f4 = Tile SF BambooTree

s1, s2, s3, s4 :: Tile
s1 = Tile SS Spring
s2 = Tile SS Summer
s3 = Tile SS Autumn
s4 = Tile SS Winter

a1, a2, a3, a4 :: Tile
a1 = Tile SA Cat
a2 = Tile SA Mouse
a3 = Tile SA Cockerel
a4 = Tile SA Centipede


-------------------------------------------------------------------------------
-- Tile collections
-------------------------------------------------------------------------------

-- | List of coin tiles.
coins :: [Tile]
coins = map (Tile SC) [One ..]

-- | List of bamboo tiles.
bamboos :: [Tile]
bamboos = map (Tile SB) [One ..]

-- | List of character tiles.
characters :: [Tile]
characters = map (Tile SK) [One ..]

-- | List of wind tiles.
winds :: [Tile]
winds = map (Tile SW) [East ..]

-- | List of dragon tiles.
dragons :: [Tile]
dragons = map (Tile SD) [Red ..]

-- | List of flower tiles.
flowers :: [Tile]
flowers = map (Tile SF) [PlumBlossom ..]

-- | List of season tiles.
seasons :: [Tile]
seasons = map (Tile SS) [Spring ..]

-- | List of animal tiles.
animals :: [Tile]
animals = map (Tile SA) [Cat ..]

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

-- | List of bonus tiles with Animals tile included.
extras :: [Tile]
extras = bonuses ++ animals

-- | List of red tiles.
reds :: [Tile]
reds = map (Tile SB) [One, Five, Seven, Nine] ++ [Tile SD Red]

-- | List of green tiles.
greens :: [Tile]
greens = map (Tile SB) [Two, Three, Four, Six, Eight] ++ [Tile SD Green]

-- | List of blue tiles.
blues :: [Tile]
blues = [Tile SC Eight] ++ winds ++ [Tile SD White]

-- | List of all regular tiles without bonus tiles.
regulars :: [Tile]
regulars   = coins ++ bamboos ++ characters ++ winds ++ dragons

-- | List containing all tiles tiles without Animal tiles.
allTiles :: [Tile]
allTiles   = regulars ++ bonuses


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
demote SA = Animal

-- | Gets the type of tile.
tileType :: Tile -> TileType
tileType (Tile tt _) = demote tt

-- | Gets the numeric value of the tile.
tileValue :: Tile -> Int
tileValue (Tile _ tv) = fromEnum tv + 1

-- | Is the value of a suit tile 8 or 9?
isEightOrNine :: Tile -> Bool
isEightOrNine (Tile tt tv) =
  case tt of
    SC -> eightOrNine tv
    SB -> eightOrNine tv
    SK -> eightOrNine tv
    _  -> False
  where
    eightOrNine tv = tv == Eight || tv == Nine

-- | Check if the list of tiles have the same tile type
isSameTileType :: [Tile] -> Bool
isSameTileType = (==) 1 . length . nub . fmap tileType


-------------------------------------------------------------------------------
-- Wall building
-------------------------------------------------------------------------------

-- | Set of mahjong tiles.
mjSet :: [Tile]
mjSet      = (regulars >>= take 4 . repeat) ++ bonuses

-- | Creates a wall
getWall :: Int -> [Tile]
getWall a  = fst $ fisherYates (mkStdGen a) mjSet

-- | Creates an random wall
randomWall :: IO [Tile]
randomWall = do
  r <- randNumber
  return $ getWall r

-- | gets a random number
randNumber :: IO Int
randNumber = (flip mod 144 . abs) <$> randomIO

-- | Fisher Yates Algorithm
-- | Source:  http://www.haskell.org/haskellwiki/Random_shuffle
fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l  =
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate       = zip [1..]
    initial x gen  = (singleton 0 x, gen)

-- | Step function for Fisher Yates Algorithm
-- | Source:  http://www.haskell.org/haskellwiki/Random_shuffle
fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

