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
  tileType, isEightOrNine,

  -- ** Wall building
  mjSet, getWall, randomWall
) where

import Game.Mahjong.Class

import Data.Map (Map, insert, (!), elems, singleton)
import Data.Maybe (fromJust)
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

-- | The 8 kinds of tiles.
--   Really wish I can sigma type this
data Tile
  = CTile Values   -- ^ Coin Tile
  | BTile Values   -- ^ Bamboo Tile
  | KTile Values   -- ^ Character Tile
  | WTile Winds    -- ^ Wind Tile
  | DTile Dragons  -- ^ Dragon Tile
  | FTile Flowers  -- ^ Flower Tile
  | STile Seasons  -- ^ Season Tile
  | ATile Animals  -- ^ Animal Tile
    deriving (Eq, Ord, Show)


-------------------------------------------------------------------------------
-- Typeclass instances
-------------------------------------------------------------------------------

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

instance Pretty Tile where
  pp (CTile c)  = "C" ++ pp c
  pp (BTile b)  = "B" ++ pp b
  pp (KTile k)  = "K" ++ pp k
  pp (WTile w)  = "W" ++ pp w
  pp (DTile d)  = "D" ++ pp d
  pp (FTile f)  = "F" ++ pp f
  pp (STile s)  = "S" ++ pp s
  pp (ATile a)  = "A" ++ pp a

ppHelper :: (Eq a) => a -> [b] -> [a] -> b
ppHelper a reps = fromJust . lookup a . flip zip reps

instance TilePred Tile where
  isCoin                = (==) Coin      . tileType
  isBamboo              = (==) Bamboo    . tileType
  isCharacter           = (==) Character . tileType

  isSimple              = allCond [isSuit, not . isTerminal]
  isTerminal  (CTile v) = elem v [One, Nine]
  isTerminal  (BTile v) = elem v [One, Nine]
  isTerminal  (KTile v) = elem v [One, Nine]
  isTerminal  _         = False
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
  next (CTile c) = CTile $ next c
  next (BTile b) = BTile $ next b
  next (KTile k) = KTile $ next k
  next (WTile w) = WTile $ next w
  next (DTile d) = DTile $ next d
  next (FTile f) = FTile $ next f
  next (STile s) = STile $ next s
  next (ATile a) = ATile $ next a

  prev (CTile c) = CTile $ prev c
  prev (BTile b) = BTile $ prev b
  prev (KTile k) = KTile $ prev k
  prev (WTile w) = WTile $ prev w
  prev (DTile d) = DTile $ prev d
  prev (FTile f) = FTile $ prev f
  prev (STile s) = STile $ prev s
  prev (ATile a) = ATile $ prev a


-------------------------------------------------------------------------------
-- Tile Aliases
-------------------------------------------------------------------------------

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

we, ws, ww, wn :: Tile
we = WTile East
ws = WTile South
ww = WTile West
wn = WTile North

dr, dg, dw :: Tile
dr = DTile Red
dg = DTile Green
dw = DTile White

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
-- Tile collections
-------------------------------------------------------------------------------

-- | List of coin tiles.
coins :: [Tile]
coins = map CTile [One ..]

-- | List of bamboo tiles.
bamboos :: [Tile]
bamboos = map BTile [One ..]

-- | List of character tiles.
characters :: [Tile]
characters = map KTile [One ..]

-- | List of wind tiles.
winds :: [Tile]
winds = map WTile [East ..]

-- | List of dragon tiles.
dragons :: [Tile]
dragons = map DTile [Red ..]

-- | List of flower tiles.
flowers :: [Tile]
flowers = map FTile [PlumBlossom ..]

-- | List of season tiles.
seasons :: [Tile]
seasons = map STile [Spring ..]

-- | List of animal tiles.
animals :: [Tile]
animals = map ATile [Cat ..]

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
reds = map BTile [One, Five, Seven, Nine] ++ [DTile Red]

-- | List of green tiles.
greens :: [Tile]
greens = map BTile [Two, Three, Four, Six, Eight] ++ [DTile Green]

-- | List of blue tiles.
blues :: [Tile]
blues = [CTile Eight] ++ winds ++ [DTile White]

-- | List of all regular tiles without bonus tiles.
regulars :: [Tile]
regulars   = coins ++ bamboos ++ characters ++ winds ++ dragons

-- | List containing all tiles tiles without Animal tiles.
allTiles :: [Tile]
allTiles   = regulars ++ bonuses


-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

-- | Gets the type of tile.
tileType :: Tile -> TileType
tileType (CTile _) = Coin
tileType (BTile _) = Bamboo
tileType (KTile _) = Character
tileType (WTile _) = Wind
tileType (DTile _) = Dragon
tileType (FTile _) = Flower
tileType (STile _) = Season
tileType (ATile _) = Animal

-- | Is the value of a suit tile 8 or 9?
isEightOrNine :: Tile -> Bool
isEightOrNine t =
  case t of
    (CTile v) -> eightOrNine v
    (BTile v) -> eightOrNine v
    (KTile v) -> eightOrNine v
    _         -> False
  where
    eightOrNine v = v == Eight || v == Nine


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
randNumber = fmap (flip mod 144 . abs) randomIO

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

