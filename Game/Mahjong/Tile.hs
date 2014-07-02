-- |
-- Module      :  Game.Mahjong.Tile
-- Copyright   :  Joseph Ching 2014
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Tile implementation and tile related functions
module Game.Mahjong.Tile (
      -- Tile data
      SuitValue, WindValue, DragonValue, FlowerValue, SeasonValue
    , Tile(..)

      -- Tile collections
    , coins, bamboos, characters, winds, dragons, flowers, seasons
    , suits, simples, terminals, honors, edges, bonuses
    , greens, reds, blues
    , regulars, alls

      -- Tile predicates
    , isCoin, isBamboo, isCharacter, isWind, isDragon, isFlower, isSeason
    , isSuit, isSimple, isTerminal, isHonor, isEdge, isBonus
    , isGreen, isRed, isBlue
    
      -- Utility functions
    , mjSet, getWall, impureWall
    , dora, reverseDora
    ) where

import Data.List (intersperse)
import Data.Map (Map, insert, (!), elems, singleton)
import Data.Maybe (fromJust)
import System.IO.Unsafe
import System.Random


{- Data definition -}

-- | The nine suit values used for the three suits
data SuitValue = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
                 deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | The four wind values
data WindValue = East | South | West | North
                 deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | The three dragon values
data DragonValue = Red | Green | White
                   deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | The four flower values
data FlowerValue = PlumBlossom | Orchid | Chrysanthenum | BambooTree
                   deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | The four season values
data SeasonValue = Spring | Summer | Autumn | Winter
                   deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | The four animal values
data AnimalValue = Cat | Mouse | Cockerel | Centipede
                   deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Define the tiles
data Tile = Coin SuitValue
          | Bamboo SuitValue
          | Character SuitValue
          | Wind WindValue
          | Dragon DragonValue
          | Flower FlowerValue
          | Season SeasonValue
          | Animal AnimalValue
            deriving (Eq, Ord, Read)

instance Show Tile where
  show = showTile 

showTile :: Tile -> String
showTile t =
  case t of
    Coin c      -> 'C' : (getValue c [One ..])
    Bamboo b    -> 'B' : (getValue b [One ..])
    Character k -> 'K' : (getValue k [One ..])
    Wind w      -> 'W' : (getValue w [East ..])
    Dragon d    -> 'D' : (getValue d [Red ..])
    Flower f    -> 'F' : (getValue f [PlumBlossom ..])
    Season s    -> 'S' : (getValue s [Spring ..])
    Animal a    -> 'A' : (getValue a [Cat ..])
    where
      getValue :: (Eq a) => a -> [a] -> String
      getValue v = show . fromJust . lookup v . flip zip [1..]


{- Tile collections -}

coins :: [Tile]
coins = map (Coin $) [One ..]

bamboos :: [Tile]
bamboos = map (Bamboo $) [One ..] 

characters :: [Tile]
characters = map (Character $) [One ..] 

winds :: [Tile]
winds = map (Wind $) [East ..]

dragons :: [Tile]
dragons = map (Dragon $) [Red ..] 

flowers :: [Tile]
flowers = map (Flower $) [PlumBlossom ..] 

seasons :: [Tile]
seasons = map (Season $) [Spring ..] 

animals :: [Tile]
animals = map (Animal $) [Cat ..]

suits :: [Tile]
suits = coins ++ bamboos ++ characters

simples :: [Tile]
simples = concatMap (\x -> tail . init $ x) [coins, bamboos, characters]

terminals :: [Tile]
terminals = concatMap (\x -> [head x, last x]) [coins, bamboos, characters]

honors :: [Tile]
honors = winds ++ dragons

edges :: [Tile]
edges = terminals ++ honors

bonuses :: [Tile]
bonuses = flowers ++ seasons

extras :: [Tile]
extras = bonuses ++ animals

greens :: [Tile]
greens = map (Bamboo $) [Two, Three, Four, Six, Eight] ++ [Dragon Green]

reds :: [Tile]
reds = map (Bamboo $) [One, Five, Seven, Nine] ++ [Dragon Red]

blues :: [Tile]
blues = [Coin Eight] ++ winds ++ [Dragon White]

regulars :: [Tile]
regulars = coins ++ bamboos ++ characters ++ winds ++ dragons

alls :: [Tile]
alls = regulars ++ bonuses


{- Predicates for determining tile types -}

isCoin :: Tile -> Bool
isCoin (Coin _) = True
isCoin _        = False

isBamboo :: Tile -> Bool
isBamboo (Bamboo _) = True
isBamboo _          = False

isCharacter :: Tile -> Bool
isCharacter (Character _) = True
isCharacter _             = False

isWind :: Tile -> Bool
isWind (Wind _) = True
isWind _        = False

isDragon :: Tile -> Bool
isDragon (Dragon _) = True
isDragon _          = False

isFlower :: Tile -> Bool
isFlower (Flower _) = True
isFlower _          = False

isSeason :: Tile -> Bool
isSeason (Season _) = True
isSeason _          = False

isAnimal :: Tile -> Bool
isAnimal (Animal _) = True
isAnimal _          = False

isSuit :: Tile -> Bool
isSuit = or . zipWith id [isCoin, isBamboo, isCharacter] . repeat

isSimple :: Tile -> Bool
isSimple = and . zipWith id [isSuit, not . isTerminal] . repeat

isTerminal :: Tile -> Bool
isTerminal = flip elem terminals

isHonor :: Tile -> Bool
isHonor = or . zipWith id [isWind, isDragon] . repeat

isEdge :: Tile -> Bool
isEdge = or . zipWith id [isTerminal, isWind, isDragon] . repeat

isBonus :: Tile -> Bool
isBonus = or . zipWith id [isFlower, isSeason, isAnimal] . repeat

isGreen :: Tile -> Bool
isGreen = flip elem greens

isRed :: Tile -> Bool
isRed = flip elem reds

isBlue :: Tile -> Bool
isBlue = flip elem blues


{- Utility functions -}

dora :: Tile -> Tile
dora (Coin c)      = if c == Nine       then Coin One           else Coin $ succ c
dora (Bamboo b)    = if b == Nine       then Bamboo One         else Bamboo $ succ b
dora (Character k) = if k == Nine       then Character One      else Character $ succ k
dora (Wind w)      = if w == North      then Wind East          else Wind $ succ w
dora (Dragon d)    = if d == White      then Dragon Red         else Dragon $ succ d
dora (Flower f)    = if f == BambooTree then Flower PlumBlossom else Flower $ succ f
dora (Season s)    = if s == Winter     then Season Spring      else Season $ succ s
dora (Animal a)    = if a == Centipede  then Animal Cat         else Animal $ succ a

reverseDora :: Tile -> Tile
reverseDora (Coin c)      = if c == One         then Coin Nine         else Coin $ pred c
reverseDora (Bamboo b)    = if b == One         then Bamboo Nine       else Bamboo $ pred b
reverseDora (Character k) = if k == One         then Character Nine    else Character $ pred k
reverseDora (Wind w)      = if w == East        then Wind North        else Wind $ pred w
reverseDora (Dragon d)    = if d == Red         then Dragon White      else Dragon $ pred d
reverseDora (Flower f)    = if f == PlumBlossom then Flower BambooTree else Flower $ pred f
reverseDora (Season s)    = if s == Spring      then Season Winter     else Season $ pred s
reverseDora (Animal a)    = if a == Cat         then Animal Centipede  else Animal $ pred a


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
