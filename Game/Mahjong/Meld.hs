-- |
-- Module      :  Game.Mahjong.Meld
-- Copyright   :  Joseph Ching 2014
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definition for meld
--   along with methods to generate melds of certain kind
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

data Meld = Chow [Tile]   -- ^ a sequence of 3 tiles
          | Pung [Tile]   -- ^ a triple of tiles
          | Kong [Tile]   -- ^ a quartet of tiles
          | Eye [Tile]    -- ^ a pair of tiles
          | Mixed [Tile]  -- ^ for 13 orphans and 9 gates
          | Bonus [Tile]  -- ^ a set of bonus tiles
            deriving (Show, Eq)

getTiles :: Meld -> [Tile]
getTiles (Chow ts) = ts
getTiles (Pung ts) = ts
getTiles (Kong ts) = ts
getTiles (Eye ts) = ts
getTiles (Mixed ts) = ts
getTiles (Bonus ts) = ts


{- Meld generate -}

makeChow :: Tile -> Meld
makeChow t = case t of
  Wind _      -> error "Can't make chow of wind tiles"
  Dragon _    -> error "Can't make chow of dragon tiles"
  Flower _    -> error "Can't make chow of flower tiles"
  Season _    -> error "Can't make chow of season tiles"
--Animal _    -> error "Can't make chow of animal tiles"
  Coin v      -> case v of
    Eight -> error "Can't make chow with starting value Eight"
    Nine  -> error "Can't make chow with starting value Nine"
    _     -> Chow . take 3 . iterate dora $ t 
  Bamboo v    -> case v of 
    Eight -> error "Can't make chow with starting value Eight"
    Nine  -> error "Can't make chow with starting value Nine"
    _     -> Chow . take 3 . iterate dora $ t
  Character v -> case v of 
    Eight -> error "Can't make chow with starting value Eight"
    Nine  -> error "Can't make chow with starting value Nine"
    _     -> Chow . take 3 . iterate dora $ t

makePung :: Tile -> Meld
makePung t = case t of
  Flower _ -> error "Can't make pung of flower tiles"
  Season _ -> error "Can't make pung of season tiles"
--Animal _ -> error "Can't make pung of animal tiles"
  t        -> Pung $ replicate 3 t

makeKong :: Tile -> Meld
makeKong t = case t of
  Flower _ -> Kong $ flowers -- for convenience in generation only 
  Season _ -> Kong $ seasons -- not an "actual" meld
--Animal _ -> animals -- only used for 
  t        -> Kong $ replicate 4 t

makeEye :: Tile -> Meld
makeEye t = case t of
  Flower _ -> error "Can't make eye of flower tiles"
  Season _ -> error "Can't make eye of season tiles"
--Animal _ -> error "Can't make eye of animal tiles"
  t        -> Eye $ replicate 2 t

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


