-- |
-- Module      :  Game.Mahjong.Meld
-- Copyright   :  Joseph Ching 2014
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definition for meld
--   along with predicates on melds
module Game.Mahjong.Meld (
    -- Data definition 
    Status(..), Meld(..)
  , getStatus, getTiles

    -- Meld generation
  , makeChow, makePung, makeKong, makeEye, makeMixed, makeBonus

    -- Meld predicates
  , isChow, isPung, isKong, isEye, isMixed
  , isRevealed, isConcealed
  
    -- Meld predicate with respect to the tile type
  , isCoinMeld, isBambooMeld, isCharacterMeld, isWindMeld, isDragonMeld
  , isSimpleMeld, isTerminalMeld, isSuitMeld, isHonorMeld, isEdgeMeld, isBonusMeld
  , isGreenMeld, isRedMeld, isBlueMeld
  ) where

import qualified Game.Mahjong.Tile
import Game.Mahjong.Tile as T

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

isRevealed :: Status -> Bool
isRevealed = (==) Revealed

isConcealed :: Status -> Bool
isConcealed = (==) Concealed


{- Meld Predicates with Respect to Tile Type -}

isCoinMeld :: Meld -> Bool
isCoinMeld = all isCoin . getTiles

isBambooMeld :: Meld -> Bool
isBambooMeld = all isBamboo . getTiles

isCharacterMeld :: Meld -> Bool
isCharacterMeld = all isCharacter . getTiles

isWindMeld :: Meld -> Bool
isWindMeld = all isWind . getTiles

isDragonMeld :: Meld -> Bool
isDragonMeld = all isDragon . getTiles

isSimpleMeld :: Meld -> Bool
isSimpleMeld = all isSimple . getTiles

isTerminalMeld :: Meld -> Bool
isTerminalMeld = any isTerminal . getTiles

isSuitMeld :: Meld -> Bool
isSuitMeld = or . zipWith id [isCoinMeld, isBambooMeld, isCharacterMeld] . repeat

isHonorMeld :: Meld -> Bool
isHonorMeld = or . zipWith id [isWindMeld, isDragonMeld] . repeat

isEdgeMeld :: Meld -> Bool
isEdgeMeld = or . zipWith id [isTerminalMeld, isHonorMeld] . repeat

isBonusMeld :: Meld -> Bool
isBonusMeld = all T.isBonus . getTiles

isGreenMeld :: Meld -> Bool
isGreenMeld = all isGreen . getTiles

isRedMeld :: Meld -> Bool
isRedMeld = all isRed . getTiles

isBlueMeld :: Meld -> Bool
isBlueMeld = all isBlue . getTiles
