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
module Game.Mahjong.Experimental.Predicates where

import Game.Mahjong.Experimental.Meld
import Game.Mahjong.Experimental.Tile

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

instance SuitPred (Tile t) where
  isCoin                = (==) Coin . tileType
  isBamboo              = (==) Bamboo . tileType 
  isCharacter           = (==) Character . tileType 

  isSimple              = and . zipWith id [isSuit, not . isTerminal] . repeat
  isTerminal  (CTile v) = elem v [One, Nine]
  isTerminal  (BTile v) = elem v [One, Nine]
  isTerminal  (KTile v) = elem v [One, Nine]
  isTerminal  _         = False
  isSuit                = or . zipWith id [isCoin, isBamboo, isCharacter] . repeat

instance HonorPred (Tile t) where
  isWind                = (==) Wind . tileType
  isDragon              = (==) Dragon . tileType

  isHonor               = or . zipWith id [isWind, isDragon] . repeat 
  isEdge                = or . zipWith id [isTerminal, isHonor] . repeat

instance BonusPred (Tile t) where
  isFlower              = (==) Flower . tileType
  isSeason              = (==) Season . tileType
  isAnimal              = (==) Animal . tileType

  isBonus               = or . zipWith id [isFlower, isSeason, isAnimal] . repeat

instance ColorPred (Tile t) where
  isRed                 = flip elem reds . Wrap
  isGreen               = flip elem greens . Wrap
  isBlue                = flip elem blues . Wrap


-------------------------------------------------------------------------------

{- Instances for WrapTile -}

instance SuitPred WrapTile where
  isCoin      (Wrap t) = isCoin t
  isBamboo    (Wrap t) = isBamboo t
  isCharacter (Wrap t) = isCharacter t

  isSimple    (Wrap t) = isSimple t
  isTerminal  (Wrap t) = isTerminal t
  isSuit      (Wrap t) = isSuit t

instance HonorPred WrapTile where
  isWind      (Wrap t) = isWind t
  isDragon    (Wrap t) = isDragon t

  isHonor     (Wrap t) = isHonor t
  isEdge      (Wrap t) = isEdge t

instance BonusPred WrapTile where
  isFlower    (Wrap t) = isFlower t
  isSeason    (Wrap t) = isSeason t
  isAnimal    (Wrap t) = isAnimal t

  isBonus     (Wrap t) = isBonus t

instance ColorPred WrapTile where
  isRed                = flip elem reds
  isGreen              = flip elem greens
  isBlue               = flip elem blues


-------------------------------------------------------------------------------

{- Instances for Meld -}

instance SuitPred Meld where
  isCoin      = predHelper isCoin
  isBamboo    = predHelper isBamboo
  isCharacter = predHelper isCharacter

  isSimple    = and . mapWrap isSimple . meldTiles
  isTerminal  = or . mapWrap isTerminal . meldTiles
  isSuit      = predHelper isSuit

instance HonorPred Meld where
  isWind      = predHelper isWind
  isDragon    = predHelper isDragon

  isHonor     = predHelper isHonor
  isEdge      = predHelper isEdge

-- Meld doesn't have BonusPred instance

instance ColorPred Meld where
  isRed       = predHelper isRed
  isGreen     = predHelper isGreen
  isBlue      = predHelper isBlue

predHelper :: (forall a. Tile a -> Bool) -> Meld -> Bool
predHelper p = liftWrap p . head . meldTiles

