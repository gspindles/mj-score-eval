-- |
-- Module      :  Game.Mahjong.Tile
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Tile module with various functions on wall building
module Game.Mahjong.Tile
  ( -- * Tile data
    TileType
  , Tile, Tiles
  , Suit, Honor, Bonus
  , Chowable, Pungable

    -- * Constructors ':: (MetaType a) => Tile a -> WrapTile'
  , mkWrap

    -- * Tile aliases ':: WrapTile'
  , c1, c2, c3, c4, c5, c6, c7, c8, c9
  , b1, b2, b3, b4, b5, b6, b7, b8, b9
  , k1, k2, k3, k4, k5, k6, k7, k8, k9
  , w1, w2, w3, w4
  , d1, d2, d3
  , f1, f2, f3, f4
  , s1, s2, s3, s4

    -- * Tile collections ':: Tiles'
  , coins, bamboos, characters, winds, dragons, flowers, seasons, animals
  , simples, terminals, suits, honors, edges, bonuses
  , reds, greens, blues
  , regulars, allTiles

    -- * Tile predicate ':: Tile a -> Bool'
  , tileType
  , isCoinT, isBambooT, isCharacterT, isWindT, isDragonT, isFlowerT, isSeasonT
  , isSimpleT, isTerminalT, isSuitT, isHonorT, isEdgeT, isBonusT
  , isRedT, isGreenT, isBlueT

    -- * Utility functions
--, liftWrap, liftWrapT, liftWrap2
--, mapWrap, mapWrapT
  , dora, reverseDora
  , mjSet, getWall, impureWall
) where

import Data.Map (Map, insert, (!), elems, singleton)
import Game.Mahjong.Internal.Tile
import System.IO.Unsafe
import System.Random


-------------------------------------------------------------------------------

{- Wall building -}

mjSet :: Tiles
mjSet = (concatMap (take 4 . repeat) $ regulars) ++ bonuses

getWall :: Int -> Tiles
getWall a = fst $ fisherYates (mkStdGen a) mjSet

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

