{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

-- |
-- Module      :  Game.Mahjong.Internal.Predicates
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Class definitions for various sets of predicates
-- tile annd meld will implement
module Game.Mahjong.Internal.Predicates where

import Game.Mahjong.Internal.Meld
import Game.Mahjong.Internal.Tile

-------------------------------------------------------------------------------

{- Class definitions -}

-- | Predicates for suit
class SuitPred a where
  isCoin        :: a -> Bool
  isBamboo      :: a -> Bool
  isCharacter   :: a -> Bool

  isSimple      :: a -> Bool
  isTerminal    :: a -> Bool
  isSuit        :: a -> Bool

-- | Predicates for honor
class HonorPred a where
  isWind        :: a -> Bool
  isDragon      :: a -> Bool

  isHonor       :: a -> Bool
  isEdge        :: a -> Bool

-- | Predicates for bonus
class BonusPred a where
  isFlower      :: a -> Bool
  isSeason      :: a -> Bool
  isAnimal      :: a -> Bool

  isBonus       :: a -> Bool

-- | Predicates for color
class ColorPred a where
  isRed         :: a -> Bool
  isGreen       :: a -> Bool
  isBlue        :: a -> Bool


-------------------------------------------------------------------------------

{- Instances for Tile -}

instance SuitPred Tile where
  isCoin                = (==) Coin      . tileType
  isBamboo              = (==) Bamboo    . tileType 
  isCharacter           = (==) Character . tileType 

  isSimple              = and . zipWith id [isSuit, not . isTerminal]     . repeat
  isTerminal  (CTile v) = elem v [One, Nine]
  isTerminal  (BTile v) = elem v [One, Nine]
  isTerminal  (KTile v) = elem v [One, Nine]
  isTerminal  _         = False
  isSuit                = or . zipWith id [isCoin, isBamboo, isCharacter] . repeat

instance HonorPred Tile where
  isWind                = (==) Wind   . tileType
  isDragon              = (==) Dragon . tileType

  isHonor               = or . zipWith id [isWind, isDragon]    . repeat 
  isEdge                = or . zipWith id [isTerminal, isHonor] . repeat

instance BonusPred Tile where
  isFlower              = (==) Flower . tileType
  isSeason              = (==) Season . tileType
  isAnimal              = (==) Animal . tileType

  isBonus               = or . zipWith id [isFlower, isSeason, isAnimal] . repeat

instance ColorPred Tile where
  isRed                 = flip elem reds
  isGreen               = flip elem greens
  isBlue                = flip elem blues


-------------------------------------------------------------------------------

{- Instances for Meld -}

instance SuitPred Meld where
  isCoin      = all isCoin      . meldTiles
  isBamboo    = all isBamboo    . meldTiles
  isCharacter = all isCharacter . meldTiles

  isSimple    = all isSimple   . meldTiles
  isTerminal  = any isTerminal . meldTiles
  isSuit      = all isSuit     . meldTiles

instance HonorPred Meld where
  isWind      = all isWind   . meldTiles
  isDragon    = all isDragon . meldTiles

  isHonor     = all isHonor . meldTiles
  isEdge      = any isEdge  . meldTiles

-- Any bonus tile found within a meld will make this true
instance BonusPred Meld where
  isFlower    = any isFlower . meldTiles
  isSeason    = any isSeason . meldTiles
  isAnimal    = any isAnimal . meldTiles

  isBonus     = any isBonus . meldTiles

instance ColorPred Meld where
  isRed       = all isRed   . meldTiles
  isGreen     = all isGreen . meldTiles
  isBlue      = all isBlue  . meldTiles

