-- |
-- Module      :  Game.Mahjong.Internal.Class
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Class definitions for various sets of predicates
-- tile annd meld will implement
module Game.Mahjong.Internal.Class where

import Game.Mahjong.Internal.Meld
import Game.Mahjong.Internal.Tile

-------------------------------------------------------------------------------

{- Class definitions -}

class TilePred a where
  -- | Predicates for suit
  isCoin      :: a -> Bool
  isBamboo    :: a -> Bool
  isCharacter :: a -> Bool

  isSimple    :: a -> Bool
  isTerminal  :: a -> Bool
  isSuit      :: a -> Bool

  -- | Predicates for honor
  isWind      :: a -> Bool
  isDragon    :: a -> Bool

  isHonor     :: a -> Bool
  isEdge      :: a -> Bool

  -- | Predicates for bonus
  isFlower    :: a -> Bool
  isSeason    :: a -> Bool
  isAnimal    :: a -> Bool

  isBonus     :: a -> Bool

  -- | Predicates for color
  isRed       :: a -> Bool
  isGreen     :: a -> Bool
  isBlue      :: a -> Bool


-- | Instance for Tile
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


-- | Instance for Meld
instance TilePred Meld where
  isCoin      = all isCoin      . meldTiles
  isBamboo    = all isBamboo    . meldTiles
  isCharacter = all isCharacter . meldTiles

  isSimple    = all isSimple   . meldTiles
  isTerminal  = any isTerminal . meldTiles
  isSuit      = all isSuit     . meldTiles

  isWind      = all isWind   . meldTiles
  isDragon    = all isDragon . meldTiles

  isHonor     = all isHonor . meldTiles
  isEdge      = any isEdge  . meldTiles

  -- Any bonus tile found within a meld will make this true
  isFlower    = any isFlower . meldTiles
  isSeason    = any isSeason . meldTiles
  isAnimal    = any isAnimal . meldTiles

  isBonus     = any isBonus . meldTiles

  isRed       = all isRed   . meldTiles
  isGreen     = all isGreen . meldTiles
  isBlue      = all isBlue  . meldTiles


anyCond :: [(a -> Bool)] -> a -> Bool
anyCond fs x = or $ map (\f -> f x) fs

allCond :: [(a -> Bool)] -> a -> Bool
allCond fs = and . zipWith id fs . repeat


-------------------------------------------------------------------------------

{- Loop typeclass -}

class Loop a where
  next :: a -> a
  prev :: a -> a

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

instance Loop Meld where
  next m =
    case m of
      (CMeld s ts) -> meldHelper CMeld s ts next
      (PMeld s ts) -> meldHelper PMeld s ts next
      (KMeld s ts) -> meldHelper KMeld s ts next
      (EMeld s ts) -> meldHelper EMeld s ts next

  prev m =
    case m of
      (CMeld s ts) -> meldHelper CMeld s ts prev
      (PMeld s ts) -> meldHelper PMeld s ts prev
      (KMeld s ts) -> meldHelper KMeld s ts prev
      (EMeld s ts) -> meldHelper EMeld s ts prev

nextHelper :: (Bounded a, Enum a, Eq a) => a -> a
nextHelper a =
  if a == maxBound
  then minBound
  else succ a

prevHelper :: (Bounded a, Enum a, Eq a) => a -> a
prevHelper a =
  if a == minBound
  then maxBound
  else pred a

meldHelper :: (Status -> [Tile] -> Meld) -> Status -> [Tile] -> (Tile -> Tile) -> Meld
meldHelper ctor s ts f = ctor s $ map f ts

