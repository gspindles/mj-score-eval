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
    -- Data definition 
    Status(..), Meld(..)
  , getStatus, getTiles

    -- Meld generation
  , makeChow, makePung, makeKong, makeEye, makeMixed, makeBonus
  
    -- Meld predicates
  , isChow, isPung, isKong, isEye, isMixed
  ) where

import Game.Mahjong.Tile


{- Data definitions -}

data Status = Revealed
            | Concealed
            deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Meld = Chow  Status [Tile]  -- ^ a sequence of 3 tiles
          | Pung  Status [Tile]  -- ^ a triple of tiles
          | Kong  Status [Tile]  -- ^ a quartet of tiles
          | Eye   Status [Tile]  -- ^ a pair of tiles
          | Mixed        [Tile]  -- ^ for 13 orphans and 9 gates
          | Bonus        [Tile]  -- ^ a set of bonus tiles
            deriving (Show, Eq)

getStatus :: Meld -> Status
getStatus (Chow  r _) = r
getStatus (Pung  r _) = r
getStatus (Kong  r _) = r
getStatus (Eye   r _) = r
getStatus (Mixed   _) = Concealed
getStatus (Bonus   _) = Revealed

getTiles :: Meld -> [Tile]
getTiles (Chow  _ ts) = ts
getTiles (Pung  _ ts) = ts
getTiles (Kong  _ ts) = ts
getTiles (Eye   _ ts) = ts
getTiles (Mixed   ts) = ts
getTiles (Bonus   ts) = ts


{- Meld generate -}

makeChow :: Status -> Tile -> Meld
makeChow s t = case t of
  Wind _      -> error "Can't make chow of wind tiles"
  Dragon _    -> error "Can't make chow of dragon tiles"
  Flower _    -> error "Can't make chow of flower tiles"
  Season _    -> error "Can't make chow of season tiles"
--Animal _    -> error "Can't make chow of animal tiles"
  Coin v      -> case v of
    Eight -> error "Can't make chow with starting value Eight"
    Nine  -> error "Can't make chow with starting value Nine"
    _     -> Chow s . take 3 . iterate dora $ t 
  Bamboo v    -> case v of 
    Eight -> error "Can't make chow with starting value Eight"
    Nine  -> error "Can't make chow with starting value Nine"
    _     -> Chow s . take 3 . iterate dora $ t
  Character v -> case v of 
    Eight -> error "Can't make chow with starting value Eight"
    Nine  -> error "Can't make chow with starting value Nine"
    _     -> Chow s . take 3 . iterate dora $ t

makePung :: Status -> Tile -> Meld
makePung s t = case t of
  Flower _ -> error "Can't make pung of flower tiles"
  Season _ -> error "Can't make pung of season tiles"
--Animal _ -> error "Can't make pung of animal tiles"
  t        -> Pung s $ replicate 3 t

makeKong :: Status -> Tile -> Meld
makeKong s t = case t of
  Flower _ -> Kong s $ flowers -- for convenience in generation only 
  Season _ -> Kong s $ seasons -- not an "actual" meld
--Animal _ -> animals -- only used for 
  t        -> Kong s $ replicate 4 t

makeEye :: Status -> Tile -> Meld
makeEye s t = case t of
  Flower _ -> error "Can't make eye of flower tiles"
  Season _ -> error "Can't make eye of season tiles"
--Animal _ -> error "Can't make eye of animal tiles"
  t        -> Eye s $ replicate 2 t

makeMixed :: [Tile] -> Meld
makeMixed = Mixed 

makeBonus :: [Tile] -> Meld
makeBonus = Bonus


{- Meld predicates -}

isChow :: Meld -> Bool
isChow (Chow _ _) = True
isChow _          = False

isPung :: Meld -> Bool
isPung (Pung _ _) = True
isPung _          = False

isKong :: Meld -> Bool
isKong (Kong _ _) = True
isKong _          = False

isEye :: Meld -> Bool
isEye (Eye _ _)   = True
isEye _           = False

isMixed :: Meld -> Bool
isMixed (Mixed _) = True
isMixed _         = False

isBonus :: Meld -> Bool
isBonus (Bonus _) = True
isBonus _         = False


{- generate the melds -}
