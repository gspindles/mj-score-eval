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

isCoin :: Tile a -> Bool
isCoin (CTile _)      = True
isCoin _              = False

isBamboo :: Tile a -> Bool
isBamboo (BTile _)    = True
isBamboo _            = False

isCharacter :: Tile a -> Bool
isCharacter (KTile _) = True
isCharacter _         = False

isWind :: Tile a -> Bool
isWind (WTile _)      = True
isWind _              = False

isDragon :: Tile a -> Bool
isDragon (DTile _)    = True
isDragon _            = False

isFlower :: Tile a -> Bool
isFlower (FTile _)    = True
isFlower _            = False

isSeason :: Tile a -> Bool
isSeason (STile _)    = True
isSeason _            = False

isAnimal :: Tile a -> Bool
isAnimal (ATile _)    = True
isAnimal _            = False

isSimple :: Tile a -> Bool
isSimple (CTile v)    = elem v [Two .. Eight]
isSimple (BTile v)    = elem v [Two .. Eight]
isSimple (KTile v)    = elem v [Two .. Eight]
isSimple _            = False

isTerminal :: Tile a -> Bool
isTerminal (CTile v)  = elem v [One, Nine]
isTerminal (BTile v)  = elem v [One, Nine]
isTerminal (KTile v)  = elem v [One, Nine]
isTerminal _          = False

isSuit :: Tile a -> Bool
isSuit  = or . zipWith id [isCoin, isBamboo, isCharacter] . repeat

isHonor :: Tile a -> Bool
isHonor = or . zipWith id [isWind, isDragon] . repeat 

isEdge :: Tile a -> Bool
isEdge  = or . zipWith id [isTerminal, isHonor] . repeat

isBonus :: Tile a -> Bool
isBonus = or . zipWith id [isFlower, isSeason, isAnimal] . repeat

isGreen :: Tile a -> Bool
isGreen = flip elem greens . Wrap

isRed :: Tile a -> Bool
isRed   = flip elem reds . Wrap

isBlue :: Tile a -> Bool
isBlue  = flip elem blues . Wrap

