module Tile (
      TileType
    , Tile
    , showTile
    , coinTiles
    , bambooTiles
    , characterTiles
    , windTiles
    , dragonTiles
    , flowerTiles
    , seasonTiles
    , terminalTiles
    , honorTiles
    , edgeTiles
    , greenTiles
    , redTiles
    , blueTiles
    , bonusTiles
    , regularTiles
    , allTiles
    , getWall
    , isCoin
    , isBamboo
    , isCharacter
    , isWind
    , isDragon
    , isFlower
    , isSeason
    , isSuit
    , isSimple
    , isTerminal
    , isHonor
    , isEdge
    , isGreen
    , isRed
    , isBlue
    , isBonus
    , succTile
    , predTile
    )

where

import Data.List (intersperse)
import System.Random
import Data.Map (Map, insert, (!), elems, singleton)

data TileType = C | B | K | W | D | F | S | A
                deriving (Eq, Ord, Enum, Show)

type Tile = (TileType, Int)


-- | The basic tile types

showTile :: Tile -> String
showTile (t, v) = show t ++ show v

coinTiles :: [Tile]
coinTiles = [ (C, 1), (C, 2), (C, 3)
            , (C, 4), (C, 5), (C, 6)
            , (C, 7), (C, 8), (C, 9)
            ]

bambooTiles :: [Tile]
bambooTiles = [ (B, 1), (B, 2), (B, 3)
              , (B, 4), (B, 5), (B, 6)
              , (B, 7), (B, 8), (B, 9)
              ]

characterTiles :: [Tile]
characterTiles = [ (K, 1), (K, 2), (K, 3)
                 , (K, 4), (K, 5), (K, 6)
                 , (K, 7), (K, 8), (K, 9)
                 ]

windTiles :: [Tile]
windTiles = [(W, 1), (W, 2), (W, 3), (W, 4)]

dragonTiles :: [Tile]
dragonTiles = [(D, 1), (D, 2), (D, 3)]

flowerTiles :: [Tile]
flowerTiles = [(F, 1), (F, 2), (F, 3), (F, 4)]

seasonTiles :: [Tile]
seasonTiles = [(S, 1), (S, 2), (S, 3), (S, 4)]

animalTiles :: [Tile]
animalTiles = [(A, 1), (A, 2), (A, 3), (A, 4)]


-- | More complex tile types

terminalTiles :: [Tile]
terminalTiles = [(C, 1), (C, 9), (B, 1), (B, 9), (K, 1), (K, 9)]

honorTiles :: [Tile]
honorTiles = windTiles ++ dragonTiles

edgeTiles :: [Tile]
edgeTiles = terminalTiles ++ honorTiles


-- | Color tiles

greenTiles :: [Tile]
greenTiles = [(B, 2), (B, 3), (B, 4), (B, 6), (B, 8), (D, 2)]

redTiles :: [Tile]
redTiles = [(B, 1), (B, 5), (B, 7), (B, 9), (D, 1)]

blueTiles :: [Tile]
blueTiles = [(C, 8), (W, 1), (W, 2), (W, 3), (W, 4), (D, 3)]


-- | Composite tile lists

bonusTiles :: [Tile]
bonusTiles = flowerTiles ++ seasonTiles

regularTiles :: [Tile]
regularTiles = coinTiles ++ bambooTiles ++ characterTiles ++ windTiles ++ dragonTiles

allTiles :: [Tile]
allTiles = regularTiles ++ bonusTiles


-- | Wall building

mjSet :: [Tile]
mjSet = (concatMap (take 4 . repeat) $ regularTiles) ++ bonusTiles

getWall :: Int -> [Tile]
getWall a = fst $ fisherYates (mkStdGen a) mjSet


-- | Predicates for determining tile types

isCoin :: Tile -> Bool
isCoin = (== C) . fst

isBamboo :: Tile -> Bool
isBamboo = (== B) . fst

isCharacter :: Tile -> Bool
isCharacter = (== K) . fst

isWind :: Tile -> Bool
isWind = (== W) . fst

isDragon :: Tile -> Bool
isDragon = (== D) . fst

isFlower :: Tile -> Bool
isFlower = (== F) . fst

isSeason :: Tile -> Bool
isSeason = (== S) . fst

isAnimal :: Tile -> Bool
isAnimal = (== A) . fst

isSuit :: Tile -> Bool
isSuit = or . zipWith id [isCoin, isBamboo, isCharacter] . repeat

isSimple :: Tile -> Bool
isSimple = and . zipWith id [isSuit, not . isTerminal] . repeat

isTerminal :: Tile -> Bool
isTerminal = flip elem terminalTiles

isHonor :: Tile -> Bool
isHonor = or . zipWith id [isWind, isDragon] . repeat

isEdge :: Tile -> Bool
isEdge = or . zipWith id [isTerminal, isHonor] . repeat

isGreen :: Tile -> Bool
isGreen = flip elem greenTiles

isRed :: Tile -> Bool
isRed = flip elem redTiles

isBlue :: Tile -> Bool
isBlue = flip elem blueTiles

isBonus :: Tile -> Bool
isBonus = or . zipWith id [isFlower, isSeason, isAnimal] . repeat


-- | Utility functions

succTile :: Tile -> Tile
succTile t@(tt, n) | isSuit t   = if n == 9
                                  then (tt, 1)
                                  else (tt, n + 1)
                   | isWind t   = if n == 4
                                  then (tt, 1)
                                  else (tt, n + 1)
                   | isDragon t = if n == 3
                                  then (tt, 1)
                                  else (tt, n + 1)
                   | isBonus t  = if n == 4
                                  then (tt, 1)
                                  else (tt, n + 1)

predTile :: Tile -> Tile
predTile t@(tt, n) | isSuit t   = if n == 1
                                  then (tt, 9)
                                  else (tt, n - 1)
                   | isWind t   = if n == 1
                                  then (tt, 4)
                                  else (tt, n - 1)
                   | isDragon t = if n == 1
                                  then (tt, 3)
                                  else (tt, n - 1)
                   | isBonus t  = if n == 1
                                  then (tt, 4)
                                  else (tt, n - 1)


-- | Fisher Yates Algorithm

-- | Source:  http://www.haskell.org/haskellwiki/Random_shuffle
fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

-- | Source:  http://www.haskell.org/haskellwiki/Random_shuffle
fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l =
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1..]
    initial x gen = (singleton 0 x, gen)
