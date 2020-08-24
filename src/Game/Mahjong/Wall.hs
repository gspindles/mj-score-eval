-- | Wall module contains functions relating to wall building.
module Game.Mahjong.Wall (
  -- ** Include bonus tile?
  WithBonus(..),

  -- ** Wall building
  mjSet, getWall, randomWall
) where

import Game.Mahjong.Static.Tiles

import Data.Map (Map, insert, (!), elems, singleton)
import System.Random (randomR, randomIO, mkStdGen, RandomGen)

import Game.Mahjong.Tile


-------------------------------------------------------------------------------
-- Wall building
-------------------------------------------------------------------------------

-- | Whether to include bonus tiles in the game
data WithBonus
  = Exclude
  | Include
  deriving (Eq, Show)

-- | Set of mahjong tiles.
mjSet :: WithBonus -> [Tile]
mjSet Exclude = regulars >>= take 4 . repeat
mjSet Include = mjSet Exclude ++ bonuses

-- | Creates a wall
getWall :: WithBonus -> Int -> [Tile]
getWall wb a = fst . fisherYates (mkStdGen a) $ mjSet wb

-- | Creates an random wall
randomWall :: WithBonus -> IO [Tile]
randomWall wb = do
  r <- randNumber
  return $ getWall wb r

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
    initial x g    = (singleton 0 x, g)

-- | Step function for Fisher Yates Algorithm
-- | Source:  http://www.haskell.org/haskellwiki/Random_shuffle
fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

