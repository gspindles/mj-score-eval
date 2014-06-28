-- |
-- Module      :  Game.Mahjong.Meld
-- Copyright   :  Joseph Ching 2014
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

module Game.Mahjong.Meld (
    -- Meld data 
    Meld(..)

    -- Meld generation
  , makeChow, makePung, makeKong, makeEye, makeMixed, makeBonus
  
    -- Meld predicates
  , isChow, isPung, isKong, isEye, isMixed
  ) where

import Game.Mahjong.Tile


{- Data definitions -}

data Meld = Chow [Tile]
          | Pung [Tile]
          | Kong [Tile]
          | Eye [Tile]
          | Mixed [Tile]
          | Bonus [Tile]
            deriving (Show, Eq)


{- Meld generate -}

makeChow :: Tile -> Meld
makeChow = undefined

makePung :: Tile -> Meld
makePung t = case t of
  (Flower _) -> error "Can't make pung of flower tiles"
  (Season _) -> error "Can't make pung of season tiles"
--(Animal _) -> error "Can't make pung of animal tiles"
  t          -> Pung $ replicate 3 t

makeKong :: Tile -> Meld
makeKong t = case t of
  (Flower _) -> Kong $ flowers -- for convenience in generation only 
  (Season _) -> Kong $ seasons -- not an "actual" meld
--(Animal _) -> animals -- only used for 
  t          -> Kong $ replicate 4 t

makeEye :: Tile -> Meld
makeEye t = case t of
  (Flower _) -> error "Can't make eye of flower tiles"
  (Season _) -> error "Can't make eye of season tiles"
--(Animal _) -> error "Can't make eye of animal tiles"
  t          -> Eye $ replicate 2 t

makeMixed :: [Tile] -> Meld
makeMixed = Mixed 

makeBonus :: [Tile] -> Meld
makeBonus = Bonus


{- Meld predicates -}

isChow :: Meld -> Bool
isChow (Chow _)   = True
isChow _          = False

isPung :: Meld -> Bool
isPung (Pung _)   = True
isPung _          = False

isKong :: Meld -> Bool
isKong (Kong _)   = True
isKong _          = False

isEye :: Meld -> Bool
isEye (Eye _)     = True
isEye _           = False

isMixed :: Meld -> Bool
isMixed (Mixed _) = True
isMixed _         = False

isBonus :: Meld -> Bool
isBonus (Bonus _) = True
isBonus _         = False

{- generate the melds -}

