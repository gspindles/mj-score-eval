{-# LANGUAGE GADTs #-}

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
    TileType
  , Tile, Tiles
  , Suit, Honor, Bonus

    -- constructors :: (MetaType a) => Tile a -> WrapTile
  , mkWrap

    -- re-export these two small submodules
  , module Game.Mahjong.Tile.Collections
  , module Game.Mahjong.Tile.Predicates

    -- Utility functions
  , tileType
  , liftWrap, liftWrapT, liftWrap2, mapWrap, mapWrapT
  , dora, reverseDora
  , mjSet, getWall, impureWall
) where

import Data.Map (Map, insert, (!), elems, singleton)
import Game.Mahjong.Internal.Tile
import Game.Mahjong.Tile.Collections
import Game.Mahjong.Tile.Predicates
import System.IO.Unsafe
import System.Random


-------------------------------------------------------------------------------

{- Utility functions -}

dora :: Tile a -> Tile a
dora (CTile c) = if c == Nine       then CTile One         else CTile $ succ c
dora (BTile b) = if b == Nine       then BTile One         else BTile $ succ b
dora (KTile k) = if k == Nine       then KTile One         else KTile $ succ k
dora (WTile w) = if w == North      then WTile East        else WTile $ succ w
dora (DTile d) = if d == White      then DTile Red         else DTile $ succ d
dora (FTile f) = if f == BambooTree then FTile PlumBlossom else FTile $ succ f
dora (STile s) = if s == Winter     then STile Spring      else STile $ succ s
dora (ATile a) = if a == Centipede  then ATile Cat         else ATile $ succ a

reverseDora :: Tile a -> Tile a
reverseDora (CTile c) = if c == One         then CTile Nine       else CTile $ pred c
reverseDora (BTile b) = if b == One         then BTile Nine       else BTile $ pred b
reverseDora (KTile k) = if k == One         then KTile Nine       else KTile $ pred k
reverseDora (WTile w) = if w == East        then WTile North      else WTile $ pred w
reverseDora (DTile d) = if d == Red         then DTile White      else DTile $ pred d
reverseDora (FTile f) = if f == PlumBlossom then FTile BambooTree else FTile $ pred f
reverseDora (STile s) = if s == Spring      then STile Winter     else STile $ pred s
reverseDora (ATile a) = if a == Cat         then ATile Centipede  else ATile $ pred a


-------------------------------------------------------------------------------

{- Wall building -}

mjSet :: Tiles
mjSet      = (concatMap (take 4 . repeat) $ regulars) ++ bonuses

getWall :: Int -> Tiles
getWall a  = fst $ fisherYates (mkStdGen a) mjSet

impureWall :: IO Tiles
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

