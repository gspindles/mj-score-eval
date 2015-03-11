{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      :  Game.Mahjong.Internal.Try.Tile
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
module Game.Mahjong.Internal.Try.Tile where

import Data.Maybe (fromJust)


-------------------------------------------------------------------------------

{- Data definition -}

-- | The tile types
data TileTypes  = Coin | Bamboo | Character | Wind | Dragon | Flower | Season | Animal
                  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Coin tiles
data Coins      = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9
                  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Bamboo tiles
data Bamboos    = B1 | B2 | B3 | B4 | B5 | B6 | B7 | B8 | B9
                  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Character tiles
data Characters = K1 | K2 | K3 | K4 | K5 | K6 | K7 | K8 | K9
                  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Wind tiles : East, South, West, North
data Winds      = W1 | W2 | W3 | W4
                  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Dragon tiles : Red, Green, White
data Dragons    = D1 | D2 | D3
                  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Flower tiles : PlumBlossom, Orchid, Chrysanthemum, BambooTree
data Flowers    = F1 | F2 | F3 | F4 
                  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Season tiles : Spring, Summer, Autumn, Winter
data Seasons    = S1 | S2 | S3 | S4
                  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Animal tiles : Cat, Mouse, Cockerel, Centipede
data Animals    = A1 | A2 | A3 | A4
                  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Tile type
data Tile = forall a. (Show a, Tile_ a) => Tile a

-------------------------------------------------------------------------------

{- Tyleclass definitions -}

-- | Suit predicates
class SuitPred t where
  isCoin        :: t -> Bool
  isBamboo      :: t -> Bool
  isCharacter   :: t -> Bool
  isSuit        :: t -> Bool
  isSimple      :: t -> Bool
  isTerminal    :: t -> Bool

  -- Default implementation
  isCoin      _ = False
  isBamboo    _ = False
  isCharacter _ = False
  isSuit        = or  . zipWith id [isCoin, isBamboo, isCharacter] . repeat
  isSimple      = and . zipWith id [isSuit, not . isTerminal]      . repeat
  isTerminal  _ = False

-- | Honor Predicates
class (SuitPred t) => HonorPred t where
  isWind        :: t -> Bool
  isDragon      :: t -> Bool
  isHonor       :: t -> Bool
  isEdge        :: t -> Bool

  -- Default implementation
  isWind      _ = False
  isDragon    _ = False
  isHonor       = and . zipWith id [isWind, isDragon]    . repeat
  isEdge        = or  . zipWith id [isTerminal, isHonor] . repeat

-- | Bonus predicates
class BonusPred t where
  isFlower      :: t -> Bool
  isSeason      :: t -> Bool
  isAnimal      :: t -> Bool
  isBonus       :: t -> Bool

  -- Default implementation
  isFlower    _ = False
  isSeason    _ = False
  isAnimal    _ = False
  isBonus       = or  . zipWith id [isFlower, isSeason, isAnimal] . repeat

-- | Color predicates
class (HonorPred t) => ColorPred t where
  isRed         :: t -> Bool
  isGreen       :: t -> Bool
  isBlue        :: t -> Bool

  -- Default implementation
  isRed       _ = False
  isGreen     _ = False
  isBlue      _ = False


class (HonorPred t, ColorPred t, BonusPred t) => Tile_ t where
  tileType      :: t -> TileTypes
  tileRank      :: t -> Int

  dora          :: t -> t
  reverseDora   :: t -> t

  dora          = undefined
  reverseDora   = undefined

-- | 'Chowable' and 'typeclass
class Chowable t
class Pungable t


-------------------------------------------------------------------------------

{- Coin instances -}

instance SuitPred Coins where
  isCoin      _ = True
  isTerminal    = flip elem [C1, C9]

instance HonorPred Coins

instance BonusPred Coins

instance ColorPred Coins where
  isBlue        = (==) C8

instance Tile_ Coins where
  tileType    _ = Coin
  tileRank    c = 10 + showHelper c [C1 .. C9]

instance Chowable Coins
instance Pungable Coins


-------------------------------------------------------------------------------

{- Bamboo instances -}

instance SuitPred Bamboos where
  isBamboo    _ = True
  isTerminal    = flip elem [B1, B9]

instance HonorPred Bamboos

instance BonusPred Bamboos

instance ColorPred Bamboos where
  isGreen       = flip elem [B2, B3, B4, B6, B8]

instance Tile_ Bamboos where
  tileType    _ = Bamboo
  tileRank    b = 20 + showHelper b [B1 .. B9]

instance Chowable Bamboos
instance Pungable Bamboos


-------------------------------------------------------------------------------

{- Character instances -}

instance SuitPred Characters where
  isCharacter _ = True
  isTerminal    = flip elem [K1, K9]

instance HonorPred Characters

instance BonusPred Characters

instance ColorPred Characters

instance Tile_ Characters where
  tileType    _ = Character
  tileRank    k = 30 + showHelper k [K1 .. K9]

instance Chowable Characters
instance Pungable Characters


-------------------------------------------------------------------------------

{- Wind instances -}

instance SuitPred Winds

instance HonorPred Winds where
  isWind      _ = True

instance BonusPred Winds

instance ColorPred Winds where
  isBlue      _ = True

instance Tile_ Winds where
  tileType    _ = Wind
  tileRank    w = 40 + showHelper w [W1 .. W4]

instance Pungable Winds


-------------------------------------------------------------------------------

{- Dragon instances -}

instance SuitPred Dragons

instance HonorPred Dragons where
  isDragon    _ = True

instance BonusPred Dragons

instance ColorPred Dragons where
  isRed         = (==) D1
  isGreen       = (==) D2
  isBlue        = (==) D3

instance Tile_ Dragons where
  tileType    _ = Dragon
  tileRank    d = 50 + showHelper d [D1 .. D3]

instance Pungable Dragons


-------------------------------------------------------------------------------

{- Flower instances -}

instance SuitPred Flowers

instance HonorPred Flowers

instance BonusPred Flowers where
  isFlower    _ = True

instance ColorPred Flowers

instance Tile_ Flowers where
  tileType    _ = Flower
  tileRank    f = 60 + showHelper f [F1 .. F4]


-------------------------------------------------------------------------------

{- Season instances -}

instance SuitPred Seasons

instance HonorPred Seasons

instance BonusPred Seasons where
  isFlower    _ = False
  isSeason    _ = False
  isAnimal    _ = False

instance ColorPred Seasons

instance Tile_ Seasons where
  tileType    _ = Season
  tileRank    s = 70 + showHelper s [S1 .. S4]


-------------------------------------------------------------------------------

{- Animal instances -}

instance SuitPred Animals

instance HonorPred Animals

instance BonusPred Animals where
  isAnimal    _ = True

instance ColorPred Animals where

instance Tile_ Animals where
  tileType    _ = Animal
  tileRank    a = 80 + showHelper a [A1 .. A4]



-------------------------------------------------------------------------------

{- Tile instances -}

instance SuitPred Tile where
  isCoin      (Tile t) = isCoin t
  isBamboo    (Tile t) = isBamboo t
  isCharacter (Tile t) = isCharacter t
  isTerminal  (Tile t) = isTerminal t

instance HonorPred Tile where
  isWind      (Tile t) = isWind t
  isDragon    (Tile t) = isDragon t

instance BonusPred Tile where
  isFlower    (Tile t) = isFlower t
  isSeason    (Tile t) = isSeason t
  isAnimal    (Tile t) = isAnimal t

instance ColorPred Tile where
  isRed       (Tile t) = isRed t
  isGreen     (Tile t) = isGreen t
  isBlue      (Tile t) = isBlue t

instance Tile_ Tile where
  tileType    (Tile t) = tileType t
  tileRank    (Tile t) = tileRank t

instance Show Tile where
  show        (Tile t) = show t

showHelper :: (Eq a) => a -> [a] -> Int
showHelper a           = fromJust . lookup a . flip zip [1..]

instance Eq Tile where
  (==)                 = eqHelper

eqHelper :: Tile -> Tile -> Bool
eqHelper t1 t2
  | and $ map isCoin      ts = t1 == t2
  | and $ map isBamboo    ts = t1 == t2
  | and $ map isCharacter ts = t1 == t2
  | and $ map isWind      ts = t1 == t2
  | and $ map isDragon    ts = t1 == t2
  | and $ map isFlower    ts = t1 == t2
  | and $ map isSeason    ts = t1 == t2
  | and $ map isAnimal    ts = t1 == t2
  | otherwise                = False
  where
    ts :: [Tile]
    ts = [t1, t2] 

instance Ord Tile where
  compare t1 t2        = compare (tileRank t1) (tileRank t2)


-------------------------------------------------------------------------------

{- Tile Aliases -}

c1, c2, c3, c4, c5, c6, c7, c8, c9 :: Coins
c1 = C1
c2 = C2
c3 = C3
c4 = C4
c5 = C5
c6 = C6
c7 = C7
c8 = C8
c9 = C9

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: Bamboos
b1 = B1
b2 = B2
b3 = B3
b4 = B4
b5 = B5
b6 = B6
b7 = B7
b8 = B8
b9 = B9

k1, k2, k3, k4, k5, k6, k7, k8, k9 :: Characters
k1 = K1
k2 = K2
k3 = K3
k4 = K4
k5 = K5
k6 = K6
k7 = K7
k8 = K8
k9 = K9

w1, w2, w3, w4 :: Winds
w1 = W1
w2 = W2
w3 = W3
w4 = W4

d1, d2, d3 :: Dragons
d1 = D1
d2 = D2
d3 = D3

f1, f2, f3, f4 :: Flowers
f1 = F1
f2 = F2
f3 = F3
f4 = F4

s1, s2, s3, s4 :: Seasons
s1 = S1
s2 = S2
s3 = S3
s4 = S4

a1, a2, a3, a4 :: Animals
a1 = A1
a2 = A2
a3 = A3
a4 = A4


-------------------------------------------------------------------------------

{- Tile collections -}

coins, bamboos, characters, winds, dragons, flowers, seasons, animals :: [Tile]
coins      = map Tile [C1 ..]
bamboos    = map Tile [B1 ..]
characters = map Tile [K9 ..]
winds      = map Tile [W1 ..]
dragons    = map Tile [D1 ..]
flowers    = map Tile [F1 ..]
seasons    = map Tile [S1 ..]
animals    = map Tile [A1 ..]

simples, terminals, suits, honors, edges, bonuses, extras :: [Tile]
simples    = concatMap (\x -> tail . init $ x)  [coins, bamboos, characters]
terminals  = concatMap (\x -> [head x, last x]) [coins, bamboos, characters]
suits      = coins ++ bamboos ++ characters
honors     = winds ++ dragons
edges      = terminals ++ honors
bonuses    = flowers ++ seasons
extras     = bonuses ++ animals

reds, greens, blues :: [Tile]
reds       = map Tile [C1, C5, C7, C9]     ++ [Tile D1]
greens     = map Tile [C2, C3, C4, C6, C8] ++ [Tile D2]
blues      = Tile C8 : winds               ++ [Tile D3]

regulars, allTiles :: [Tile]
regulars   = coins ++ bamboos ++ characters ++ winds ++ dragons
allTiles   = regulars ++ bonuses

