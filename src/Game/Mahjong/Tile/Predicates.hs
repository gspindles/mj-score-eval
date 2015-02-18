{-# LANGUAGE GADTs #-}

-- |
-- Module      :  Game.Mahjong.Tile.Predicates
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definition of tiles
--   along with tile related functions
module Game.Mahjong.Tile.Predicates where

import Game.Mahjong.Internal.Tile
import Game.Mahjong.Tile.Collections


-------------------------------------------------------------------------------

{- Predicates for determining tile types -}

isCoin, isBamboo, isCharacter, isWind, isDragon, isFlower, isSeason, isAnimal :: Tile a -> Bool
isCoin                = (==) Coin . tileType
isBamboo              = (==) Bamboo . tileType 
isCharacter           = (==) Character . tileType 
isWind                = (==) Wind . tileType
isDragon              = (==) Dragon . tileType
isFlower              = (==) Flower . tileType
isSeason              = (==) Season . tileType
isAnimal              = (==) Animal . tileType

isSimple, isTerminal, isSuit, isHonor, isEdge, isBonus :: Tile a -> Bool
isSimple              = and . zipWith id [isSuit, not . isTerminal] . repeat
isTerminal (CTile v)  = elem v [One, Nine]
isTerminal (BTile v)  = elem v [One, Nine]
isTerminal (KTile v)  = elem v [One, Nine]
isTerminal _          = False
isSuit                = or . zipWith id [isCoin, isBamboo, isCharacter] . repeat
isHonor               = or . zipWith id [isWind, isDragon] . repeat 
isEdge                = or . zipWith id [isTerminal, isHonor] . repeat
isBonus               = or . zipWith id [isFlower, isSeason, isAnimal] . repeat

isRed, isGreen, isBlue :: Tile a -> Bool
isRed                 = flip elem reds . Wrap
isGreen               = flip elem greens . Wrap
isBlue                = flip elem blues . Wrap

