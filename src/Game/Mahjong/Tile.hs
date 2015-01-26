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
    Tile

    -- Tiles :: Tile
  , c1, c2, c3, c4, c5, c6, c7, c8, c9
  , b1, b2, b3, b4, b5, b6, b7, b8, b9
  , k1, k2, k3, k4, k5, k6, k7, k8, k9
  , w1, w2, w3, w4
  , d1, d2, d3
  , f1, f2, f3, f4
  , s1, s2, s3, s4

    -- Tile collections :: [Tile]
  , coins, bamboos, characters, winds, dragons, flowers, seasons, animals
  , simples, terminals, suits, honors, edges, bonuses, extras
  , greens, reds, blues
  , regulars, allTiles

   -- Tile predicates :: [Tile] -> [Bool]
  , isCoin, isBamboo, isCharacter, isWind, isDragon, isFlower, isSeason, isAnimal
  , isSimple, isTerminal, isSuit, isHonor, isEdge, isBonus
  , isGreen, isRed, isBlue

    -- Utility functions
  , mjSet, getWall, impureWall
  , dora, reverseDora
) where

import Data.Map (Map, insert, (!), elems, singleton)
import Data.Maybe (fromJust)
import System.IO.Unsafe
import System.Random


{- Data definition -}

-- | The three suit types
data Suit = Coin | Bamboo | Character
            deriving (Bounded, Enum, Eq, Ord, Show)

-- | The nine suit values used for the three suits
data Value = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
             deriving (Bounded, Enum, Eq, Ord, Show)

-- | The four wind values
data Wind = East | South | West | North
            deriving (Bounded, Enum, Eq, Ord, Show)

-- | The three dragon values
data Dragon = Red | Green | White
              deriving (Bounded, Enum, Eq, Ord, Show)

-- | The four flower values
data Flower = PlumBlossom | Orchid | Chrysanthemum | BambooTree
             deriving (Bounded, Enum, Eq, Ord, Show)

-- | The four season values
data Season = Spring | Summer | Autumn | Winter
              deriving (Bounded, Enum, Eq, Ord, Show)

-- | The four animal values
data Animal = Cat | Mouse | Cockerel | Centipede
              deriving (Bounded, Enum, Eq, Ord, Show)

-- | Define honor tiles
data Honor = WindTile Wind
           | DragonTile Dragon
             deriving (Eq)

-- | Define the bonus tiles
data Bonus = FlowerTile Flower
           | SeasonTile Season
           | AnimalTile Animal
             deriving (Eq)

-- | Define the tiles
data Tile = SuitTile Suit Value
          | HonorTile Honor
          | BonusTile Bonus
            deriving (Eq)

instance Show Tile where
  show = showTile

showTile :: Tile -> String
showTile (SuitTile s v) =
  case s of
    Coin      -> 'C' : (getValue v [One ..])
    Bamboo    -> 'B' : (getValue v [One ..])
    Character -> 'K' : (getValue v [One ..])
showTile (HonorTile h) =
  case h of
    WindTile w   -> 'W' : (getValue w [East ..])
    DragonTile d -> 'D' : (getValue d [Red ..])
showTile (BonusTile b) =
  case b of
    FlowerTile f -> 'F' : (getValue f [PlumBlossom ..])
    SeasonTile s -> 'S' : (getValue s [Spring ..])
    AnimalTile a -> 'A' : (getValue a [Cat ..])

getValue :: (Eq a) => a -> [a] -> String
getValue v = show . fromJust . lookup v . flip zip [1..]


{- Tile Aliases -}

c1, c2, c3, c4, c5, c6, c7, c8, c9 :: Tile
c1 = SuitTile Coin One
c2 = SuitTile Coin Two
c3 = SuitTile Coin Three
c4 = SuitTile Coin Four
c5 = SuitTile Coin Five
c6 = SuitTile Coin Six
c7 = SuitTile Coin Seven
c8 = SuitTile Coin Eight
c9 = SuitTile Coin Nine

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: Tile
b1 = SuitTile Bamboo One
b2 = SuitTile Bamboo Two
b3 = SuitTile Bamboo Three
b4 = SuitTile Bamboo Four
b5 = SuitTile Bamboo Five
b6 = SuitTile Bamboo Six
b7 = SuitTile Bamboo Seven
b8 = SuitTile Bamboo Eight
b9 = SuitTile Bamboo Nine

k1, k2, k3, k4, k5, k6, k7, k8, k9 :: Tile
k1 = SuitTile Character One
k2 = SuitTile Character Two
k3 = SuitTile Character Three
k4 = SuitTile Character Four
k5 = SuitTile Character Five
k6 = SuitTile Character Six
k7 = SuitTile Character Seven
k8 = SuitTile Character Eight
k9 = SuitTile Character Nine

w1, w2, w3, w4 :: Tile
w1 = HonorTile $ WindTile $ East
w2 = HonorTile $ WindTile $ South
w3 = HonorTile $ WindTile $ West
w4 = HonorTile $ WindTile $ North

d1, d2, d3 :: Tile
d1 = HonorTile $ DragonTile $ Red
d2 = HonorTile $ DragonTile $ Green
d3 = HonorTile $ DragonTile $ White

f1, f2, f3, f4 :: Tile
f1 = BonusTile $ FlowerTile $ PlumBlossom
f2 = BonusTile $ FlowerTile $ Orchid
f3 = BonusTile $ FlowerTile $ Chrysanthemum
f4 = BonusTile $ FlowerTile $ BambooTree

s1, s2, s3, s4 :: Tile
s1 = BonusTile $ SeasonTile $ Spring
s2 = BonusTile $ SeasonTile $ Summer
s3 = BonusTile $ SeasonTile $ Autumn
s4 = BonusTile $ SeasonTile $ Winter

a1, a2, a3, a4 :: Tile
a1 = BonusTile $ AnimalTile $ Cat
a2 = BonusTile $ AnimalTile $ Mouse
a3 = BonusTile $ AnimalTile $ Cockerel
a4 = BonusTile $ AnimalTile $ Centipede


{- Tile collections -}

coins :: [Tile]
coins      = map (uncurry SuitTile) . flip zip [One ..] . repeat $ Coin

bamboos :: [Tile]
bamboos    = map (uncurry SuitTile) . flip zip [One ..] . repeat $ Bamboo

characters :: [Tile]
characters = map (uncurry SuitTile) . flip zip [One ..] . repeat $ Character

winds :: [Tile]
winds      = map (HonorTile . WindTile) [East ..]

dragons :: [Tile]
dragons    = map (HonorTile . DragonTile) [Red ..]

flowers :: [Tile]
flowers    = map (BonusTile . FlowerTile) [PlumBlossom ..]

seasons :: [Tile]
seasons    = map (BonusTile . SeasonTile) [Spring ..]

animals :: [Tile]
animals    = map (BonusTile . AnimalTile) [Cat ..]

simples :: [Tile]
simples    = concatMap (\x -> tail . init $ x) [coins, bamboos, characters]
--         = map (uncurry SuitTile) [ (t, v) | t <- [Coin..], v <- [Two..Eight] ]

terminals :: [Tile]
terminals  = concatMap (\x -> [head x, last x]) [coins, bamboos, characters]
--         = map (uncurry SuitTile) [ (t, v) | t <- [Coin..], v <- [One, Nine] ]

suits :: [Tile]
suits      = coins ++ bamboos ++ characters

honors :: [Tile]
honors     = winds ++ dragons

edges :: [Tile]
edges      = terminals ++ honors

bonuses :: [Tile]
bonuses    = flowers ++ seasons

extras :: [Tile]
extras     = bonuses ++ animals

greens :: [Tile]
greens     = map (uncurry SuitTile) (zip (repeat Bamboo) [Two, Three, Four, Six, Eight])
             ++ [HonorTile $ DragonTile $ Green]

reds :: [Tile]
reds       = map (uncurry SuitTile) (zip (repeat Bamboo) [One, Five, Seven, Nine])
             ++ [HonorTile $ DragonTile Red]

blues :: [Tile]
blues      = [SuitTile Coin Eight] ++ winds ++ [HonorTile $ DragonTile $ White]

regulars :: [Tile]
regulars   = coins ++ bamboos ++ characters ++ winds ++ dragons

allTiles :: [Tile]
allTiles   = regulars ++ bonuses


{- Predicates for determining tile types -}

isCoin :: Tile -> Bool
isCoin (SuitTile Coin _)            = True
isCoin _                            = False

isBamboo :: Tile -> Bool
isBamboo (SuitTile Bamboo _)        = True
isBamboo _                          = False

isCharacter :: Tile -> Bool
isCharacter (SuitTile Character _)  = True
isCharacter _                       = False

isWind :: Tile -> Bool
isWind (HonorTile (WindTile _))     = True
isWind _                            = False

isDragon :: Tile -> Bool
isDragon (HonorTile (DragonTile _)) = True
isDragon _                          = False

isFlower :: Tile -> Bool
isFlower (BonusTile (FlowerTile _)) = True
isFlower _                          = False

isSeason :: Tile -> Bool
isSeason (BonusTile (SeasonTile _)) = True
isSeason _                          = False

isAnimal :: Tile -> Bool
isAnimal (BonusTile (AnimalTile _)) = True
isAnimal _                          = False

isSimple :: Tile -> Bool
isSimple (SuitTile _ v)             = elem v [Two .. Eight]
isSimple _                          = False

isTerminal :: Tile -> Bool
isTerminal (SuitTile _ v)           = elem v [One, Nine]
isTerminal _                        = False

isSuit :: Tile -> Bool
isSuit (SuitTile _ _)               = True
isSuit _                            = False

isHonor :: Tile -> Bool
isHonor (HonorTile _)               = True
isHonor _                           = False

isEdge :: Tile -> Bool
isEdge                              = or . zipWith id [isTerminal, isHonor] . repeat

isBonus :: Tile -> Bool
isBonus (BonusTile _)               = True
isBonus _                           = False

isGreen :: Tile -> Bool
isGreen                             = flip elem greens

isRed :: Tile -> Bool
isRed                               = flip elem reds

isBlue :: Tile -> Bool
isBlue                              = flip elem blues


{- Utility functions -}

dora :: Tile -> Tile
dora (SuitTile s v) = if v == Nine 
                      then SuitTile s One
                      else SuitTile s $ succ v
dora (HonorTile h)  = case h of
  (WindTile w)     -> if w == North
                      then HonorTile $ WindTile East
                      else HonorTile $ WindTile $ succ w
  (DragonTile d)   -> if d == White
                      then HonorTile $ DragonTile Red
                      else HonorTile $ DragonTile $ succ d
dora (BonusTile b) =  case b of
  (FlowerTile f)   -> if f == BambooTree
                      then BonusTile $ FlowerTile PlumBlossom
                      else BonusTile $ FlowerTile $ succ f
  (SeasonTile s)   -> if s == Winter
                      then BonusTile $ SeasonTile Spring
                      else BonusTile $ SeasonTile $ succ s
  (AnimalTile a)   -> if a == Centipede
                      then BonusTile $ AnimalTile Cat
                      else BonusTile $ AnimalTile $ succ a

reverseDora :: Tile -> Tile
reserseDora (SuitTile s v) = if v == One 
                             then SuitTile s Nine
                             else SuitTile s $ pred v
reverseDora (HonorTile h)  = case h of
  (WindTile w)            -> if w == East
                             then HonorTile $ WindTile North
                             else HonorTile $ WindTile $ pred w
  (DragonTile d)          -> if d == Red
                             then HonorTile $ DragonTile White
                             else HonorTile $ DragonTile $ pred d
reverseDora (BonusTile b)  = case b of
  (FlowerTile f)          -> if f == PlumBlossom
                             then BonusTile $ FlowerTile BambooTree
                             else BonusTile $ FlowerTile $ pred f
  (SeasonTile s)          -> if s == Spring
                             then BonusTile $ SeasonTile Winter
                             else BonusTile $ SeasonTile $ pred s
  (AnimalTile a)          -> if a == Cat
                             then BonusTile $ AnimalTile Centipede
                             else BonusTile $ AnimalTile $ pred a


{- Wall building -}

mjSet :: [Tile]
mjSet      = (concatMap (take 4 . repeat) $ regulars) ++ bonuses

getWall :: Int -> [Tile]
getWall a  = fst $ fisherYates (mkStdGen a) mjSet

impureWall :: IO [Tile]
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
