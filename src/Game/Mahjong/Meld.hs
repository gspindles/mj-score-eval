-- |
-- Module      :  Game.Mahjong.Meld
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definition for meld
--   along with predicates on melds
module Game.Mahjong.Meld (
    -- Data definition 
    MeldType(..), Status(..)
  , Meld, meldType, meldStatus, meldTiles
  , Bonus, bonusTiles
  , OnHand, onHandTiles

    -- Meld generation
  , makeChow, makePung, makeKong, makeEye

    -- Meld predicates
  , isChow, isPung, isKong, isEye
  , isRevealed, isConcealed
  
    -- Meld predicate with respect to the tile type
  , isCoinMeld, isBambooMeld, isCharacterMeld, isWindMeld, isDragonMeld
  , isSimpleMeld, isTerminalMeld, isSuitMeld, isHonorMeld, isEdgeMeld
  , isGreenMeld, isRedMeld, isBlueMeld
  ) where

import Game.Mahjong.Tile
import Data.List (intersperse)

{- Data definitions -}

data MeldType = Chow | Pung | Kong | Eye
                deriving (Eq, Read, Show)

-- | R: Revealed, H: Concealed
data Status = R | H
              deriving (Eq, Read, Show)

data Meld = Meld
          { meldType :: MeldType
          , meldStatus :: Status
          , meldTiles :: [Tile]
          } deriving (Eq, Read)

newtype Bonus = Bonus { bonusTiles :: [Tile] }
             deriving (Eq, Read)

newtype OnHand = OnHand { onHandTiles :: [Tile] }
                 deriving (Eq, Read)

-- | show unfinished stuff (), then revealed <>, then concealed [], then bonus {}
instance Show Meld where
  show (Meld _ R ts) = "<" ++ join ts ++ ">"
  show (Meld _ H ts) = "[" ++ join ts ++ "]"

instance Show Bonus where
  show (Bonus ts) = "{" ++ join ts ++ "}"

instance Show OnHand where
  show (OnHand ts) = "(" ++ join ts ++ ")"

join :: [Tile] -> String
join = concat . intersperse "," . map show


{- Predicates for determining meld types -}

isChow :: Meld -> Bool
isChow (Meld Chow _ _) = True
isChow _               = False

-- | Kong do count as pung during scoring process
isPung :: Meld -> Bool
isPung (Meld Pung _ _) = True
isPung (Meld Kong _ _) = True
isPung _               = True

isKong :: Meld -> Bool
isKong (Meld Kong _ _) = True
isKong _               = False

isEye :: Meld -> Bool
isEye (Meld Eye _ _) = True
isEye _              = False

isRevealed :: Meld -> Bool
isRevealed (Meld _ R _) = True
isRevealed _            = False

isConcealed :: Meld -> Bool
isConcealed (Meld _ H _) = True
isConcealed _            = False


{- Meld generation -}

makeChow :: Status -> Tile -> Either String Meld
makeChow s t =
  if not . isSuitTile $ t
  then Left $ "can't make chow of " ++ name (tileType t) ++ " tiles."
  else if or . zipWith (==) [8, 9] . repeat . tileValue $ t
       then Right . Meld Chow s $ neighbor t
       else Right . Meld Chow s . take 3 . iterate dora $ t
       where neighbor :: Tile -> [Tile]
             neighbor = zipWith id [reverseDora, id, dora] . repeat

makePung :: Status -> Tile -> Either String Meld
makePung s t =
  if isBonusTile t
  then Left $ "Can't make pung of " ++ name (tileType t) ++ " tiles."
  else Right . Meld Pung s $ replicate 3 t

makeKong :: Status -> Tile -> Either String Meld
makeKong s t =
  if isBonusTile t
  then Left $ "Can't make kong of " ++ name (tileType t) ++ " tiles."
  else Right . Meld Kong s $ replicate 4 t

makeEye :: Status -> Tile -> Either String Meld
makeEye s t =
  if isBonusTile t
  then Left $ "Can't make eye of " ++ name (tileType t) ++ " tiles."
  else Right . Meld Eye s $ replicate 2 t


{- Meld Predicates with Respect to Tile Type -}

isCoinMeld :: Meld -> Bool
isCoinMeld = all isCoinTile . meldTiles

isBambooMeld :: Meld -> Bool
isBambooMeld = all isBambooTile . meldTiles

isCharacterMeld :: Meld -> Bool
isCharacterMeld = all isCharacterTile . meldTiles

isWindMeld :: Meld -> Bool
isWindMeld = all isWindTile . meldTiles

isDragonMeld :: Meld -> Bool
isDragonMeld = all isDragonTile . meldTiles

isSimpleMeld :: Meld -> Bool
isSimpleMeld = all isSimpleTile . meldTiles

isTerminalMeld :: Meld -> Bool
isTerminalMeld = any isTerminalTile . meldTiles

isSuitMeld :: Meld -> Bool
isSuitMeld = or . zipWith id [isCoinMeld, isBambooMeld, isCharacterMeld] . repeat

isHonorMeld :: Meld -> Bool
isHonorMeld = or . zipWith id [isWindMeld, isDragonMeld] . repeat

isEdgeMeld :: Meld -> Bool
isEdgeMeld = or . zipWith id [isTerminalMeld, isHonorMeld] . repeat

isGreenMeld :: Meld -> Bool
isGreenMeld = all isGreenTile . meldTiles

isRedMeld :: Meld -> Bool
isRedMeld = all isRedTile . meldTiles

isBlueMeld :: Meld -> Bool
isBlueMeld = all isBlueTile . meldTiles
