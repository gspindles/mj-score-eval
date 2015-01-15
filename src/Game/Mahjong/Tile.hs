-- |
-- Module      :  Game.Mahjong.Tile
-- Copyright   :  Joseph Ching 2014
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definition of tiles
--   along with tile related functions
module Game.Mahjong.Tile (
      -- Tile data
      TileType(..), Tile, tileType, tileValue, Name(..)

      -- Tile collections
    , coins, bamboos, characters, winds, dragons, flowers, seasons, animals
    , simples, terminals, suits, honors, edges, bonuses, extras
    , greens, reds, blues
    , regulars, allTiles

      -- Tile predicates
    , isCoinTile, isBambooTile, isCharacterTile, isWindTile, isDragonTile, isFlowerTile, isSeasonTile, isAnimalTile
    , isSimpleTile, isTerminalTile, isSuitTile, isHonorTile, isEdgeTile, isBonusTile
    , isGreenTile, isRedTile, isBlueTile

      -- Utility functions
    , mjSet, getWall, impureWall
    , dora, reverseDora
    ) where

import Data.Map (Map, insert, (!), elems, singleton)
import System.IO.Unsafe
import System.Random


{- Data definition -}

-- | The tile types: Coin, Bamboo, Character, Wind, Dragon, Flower, Season, Animal
data TileType = C | B | K | W | D | F | S | A
                deriving (Eq, Read, Show)

-- | Define the tiles
data Tile = Tile
          { tileType :: TileType
          , tileValue :: Int
          } deriving (Eq, Read)

instance Show Tile where
  show (Tile tt tv) = show tt ++ show tv


{- Class definition -}

class Name a where
  name :: a -> String

instance Name TileType where
  name C = "Coin"
  name B = "Bamboo"
  name K = "Character"
  name W = "Wind"
  name D = "Dragon"
  name F = "Season"
  name A = "Animal"


{- Tile collections -}

coins :: [Tile]
coins = map (Tile C $) [1..9]

bamboos :: [Tile]
bamboos = map (Tile B $) [1..9]

characters :: [Tile]
characters = map (Tile K $) [1..9]

-- | 1: East, 2: South, 3: North, 4: West
winds :: [Tile]
winds = map (Tile W $) [1..4]

-- | 1: Red, 2: Green, 3: White
dragons :: [Tile]
dragons = map (Tile D $) [1..3]

-- | 1: Plum Blossom, 2: Orchid, 3: Chrysanthemum, 4: Bamboo Tree
flowers :: [Tile]
flowers = map (Tile F $) [1..4]

-- | 1: Spring, 2: Summer, 3: Autumn, 4: Winter
seasons :: [Tile]
seasons = map (Tile S $) [1..4]

-- | 1: Cat, 2: Mouse, 3: Cockerel, 4: Centipede
animals :: [Tile]
animals = map (Tile A $) [1..4]

simples :: [Tile]
simples = concatMap (\x -> tail . init $ x) [coins, bamboos, characters]

terminals :: [Tile]
terminals = concatMap (\x -> [head x, last x]) [coins, bamboos, characters]

suits :: [Tile]
suits = coins ++ bamboos ++ characters

honors :: [Tile]
honors = winds ++ dragons

edges :: [Tile]
edges = terminals ++ honors

bonuses :: [Tile]
bonuses = flowers ++ seasons

extras :: [Tile]
extras = bonuses ++ animals

greens :: [Tile]
greens = map (Tile B $) [2, 3, 4, 6, 8] ++ [Tile D 2]

reds :: [Tile]
reds = map (Tile B $) [1, 5, 7, 9] ++ [Tile D 1]

blues :: [Tile]
blues = [Tile C 8] ++ winds ++ [Tile D 3]

regulars :: [Tile]
regulars = coins ++ bamboos ++ characters ++ winds ++ dragons

allTiles :: [Tile]
allTiles = regulars ++ bonuses


{- Predicates for determining tile types -}

isCoinTile :: Tile -> Bool
isCoinTile (Tile C _) = True
isCoinTile _          = False

isBambooTile :: Tile -> Bool
isBambooTile (Tile B _) = True
isBambooTile _          = False

isCharacterTile :: Tile -> Bool
isCharacterTile (Tile K _) = True
isCharacterTile _          = False

isWindTile :: Tile -> Bool
isWindTile (Tile W _) = True
isWindTile _          = False

isDragonTile :: Tile -> Bool
isDragonTile (Tile D _) = True
isDragonTile _          = False

isFlowerTile :: Tile -> Bool
isFlowerTile (Tile F _) = True
isFlowerTile _          = False

isSeasonTile :: Tile -> Bool
isSeasonTile (Tile S _) = True
isSeasonTile _          = False

isAnimalTile :: Tile -> Bool
isAnimalTile (Tile A _) = True
isAnimalTile _          = False

isSimpleTile :: Tile -> Bool
isSimpleTile = and . zipWith id [isSuitTile, not . isTerminalTile] . repeat

isTerminalTile :: Tile -> Bool
isTerminalTile = flip elem terminals

isSuitTile :: Tile -> Bool
isSuitTile = or . zipWith id [isCoinTile, isBambooTile, isCharacterTile] . repeat

isHonorTile :: Tile -> Bool
isHonorTile = or . zipWith id [isWindTile, isDragonTile] . repeat

isEdgeTile :: Tile -> Bool
isEdgeTile = or . zipWith id [isTerminalTile, isWindTile, isDragonTile] . repeat

isBonusTile :: Tile -> Bool
isBonusTile = or . zipWith id [isFlowerTile, isSeasonTile, isAnimalTile] . repeat

isGreenTile :: Tile -> Bool
isGreenTile = flip elem greens

isRedTile :: Tile -> Bool
isRedTile = flip elem reds

isBlueTile :: Tile -> Bool
isBlueTile = flip elem blues


{- Utility functions -}

dora :: Tile -> Tile
dora (Tile C c) = if c == 9 then Tile C 1 else Tile C (succ c)
dora (Tile B b) = if b == 9 then Tile B 1 else Tile B (succ b)
dora (Tile K k) = if k == 9 then Tile K 1 else Tile K (succ k)
dora (Tile W w) = if w == 4 then Tile W 1 else Tile W (succ w)
dora (Tile D d) = if d == 3 then Tile D 1 else Tile D (succ d)
dora (Tile F f) = if f == 4 then Tile F 1 else Tile F (succ f)
dora (Tile S s) = if s == 4 then Tile S 1 else Tile S (succ s)
dora (Tile A a) = if a == 4 then Tile A 1 else Tile A (succ a)

reverseDora :: Tile -> Tile
reverseDora (Tile C c) = if c == 1 then Tile C 9 else Tile C (pred c)
reverseDora (Tile B b) = if b == 1 then Tile B 9 else Tile B (pred b)
reverseDora (Tile K k) = if k == 1 then Tile K 9 else Tile K (pred k)
reverseDora (Tile W w) = if w == 1 then Tile W 4 else Tile W (pred w)
reverseDora (Tile D d) = if d == 1 then Tile D 3 else Tile D (pred d)
reverseDora (Tile F f) = if f == 1 then Tile F 4 else Tile F (pred f)
reverseDora (Tile S s) = if s == 1 then Tile S 4 else Tile S (pred s)
reverseDora (Tile A a) = if a == 1 then Tile A 4 else Tile A (pred a)


{- Wall building -}

mjSet :: [Tile]
mjSet = (concatMap (take 4 . repeat) $ regulars) ++ bonuses

getWall :: Int -> [Tile]
getWall a = fst $ fisherYates (mkStdGen a) mjSet

impureWall :: [Tile]
impureWall = getWall impureRandNumber

impureRandNumber :: Int
impureRandNumber = mod (unsafePerformIO randomIO) 144

-- | Fisher Yates Algorithm
-- | Source:  http://www.haskell.org/haskellwiki/Random_shuffle
fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l =
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1..]
    initial x gen = (singleton 0 x, gen)
