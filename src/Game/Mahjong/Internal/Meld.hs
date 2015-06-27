-- |
-- Module      :  Game.Mahjong.Internal.Meld
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definitions and instances for meld
--   along with predicates on melds
module Game.Mahjong.Internal.Meld where

import Game.Mahjong.Internal.Tile
import Data.List (intercalate)


-------------------------------------------------------------------------------

{- Data definitions -}

-- | R: Revealed, H: Concealed
data Status   = Revealed | Concealed
                deriving (Bounded, Enum, Eq, Ord)

-- | Meld types
data MeldType = Chow | Pung | Kong | Eyes
                deriving (Eq, Show)

-- | Meld data
data Meld     = CMeld { status :: Status, meldTiles :: [Tile] }
              | PMeld { status :: Status, meldTiles :: [Tile] }
              | KMeld { status :: Status, meldTiles :: [Tile] }
              | EMeld { status :: Status, meldTiles :: [Tile] }
                deriving (Eq)


-------------------------------------------------------------------------------

{- Class instances -}

instance Show Status where
  show Revealed         = "\\"
  show Concealed        = "/"

-- | show unfinished stuff (), then revealed <>, then concealed [], then bonus {}
instance Show Meld where
  show (CMeld s ts)     = show s ++ join' " " ts ++ ">"
  show (PMeld s ts)     = show s ++ join' " " ts ++ "]"
  show (KMeld s ts)     = show s ++ join' " " ts ++ "}"
  show (EMeld s ts)     = show s ++ join' " " ts ++ ")"

  showList ms _         = intercalate "  " . map show $ ms

join' :: Show a => String -> [a] -> String
join' delim             = intercalate delim . map show


-------------------------------------------------------------------------------

{- Safe meld generation -}

-- | Given a suit tile, takes 2 successors and a chow.
-- In the case Eight or Nine is provided, the chow will just be Seven Eight Nine.
-- Given a non suit tile, return an error message.
mkChow :: Status -> Tile -> Maybe Meld
mkChow s t              =
  case t of
    (CTile c)           -> Just $ chowHelper CTile s c
    (BTile b)           -> Just $ chowHelper BTile s b
    (KTile k)           -> Just $ chowHelper KTile s k
    _                   -> Nothing

chowHelper :: (Values -> Tile) -> Status -> Values -> Meld
chowHelper tCtor s v =
  if elem v [Eight, Nine]
  then CMeld s . map tCtor $ [Seven, Eight, Nine]
  else CMeld s . map tCtor . take 3 . iterate succ $ v

mkPung, mkKong, mkEyes :: Status -> Tile -> Maybe Meld
mkPung s t              = meldHelper1 PMeld s t 3
mkKong s t              = meldHelper1 KMeld s t 4
mkEyes s t              = meldHelper1 EMeld s t 2

meldHelper1 :: (Status -> [Tile] -> Meld)
           -> Status -> Tile -> Int
           -> Maybe Meld
meldHelper1 mCtor s t n =
  case t of
    (FTile _)           -> Nothing
    (STile _)           -> Nothing
    (ATile _)           -> Nothing
    _                   -> Just $ meldHelper2 mCtor s t n

meldHelper2 :: (Status -> [Tile] -> Meld)
           -> Status -> Tile -> Int
           -> Meld
meldHelper2 mCtor s t n = mCtor s $ replicate n t


-------------------------------------------------------------------------------

{- Predicates for determining status -}

isConcealed, isRevealed :: Meld -> Bool
isConcealed = (==) Concealed . status
isRevealed  = (==) Revealed  . status


-------------------------------------------------------------------------------

{- Predicates for determining meld types -}

isChow, isPung, isKong, isEyes :: Meld -> Bool
isChow = (== Chow) . meldType
-- kong does get counted as pung
isPung = or . zipWith id [(== Pung), (== Kong)] . repeat . meldType
isKong = (== Kong) . meldType
isEyes = (== Chow) . meldType


-------------------------------------------------------------------------------

{- Utilities functions -}

meldType :: Meld -> MeldType
meldType (CMeld _ _) = Chow
meldType (PMeld _ _) = Pung
meldType (KMeld _ _) = Kong
meldType (EMeld _ _) = Eyes


shiftMeld :: Meld -> Meld
shiftMeld m =
  case m of
    (CMeld s ts) ->
      case minimum ts of
        (CTile c)   -> shiftHelper CTile c
        (BTile b)   -> shiftHelper BTile b
        (KTile k)   -> shiftHelper KTile k
      where
        shiftHelper :: (Values -> Tile) -> Values -> Meld
        shiftHelper tCtor v =
          if v < Seven
          then chowHelper tCtor s $ succ v
          else chowHelper tCtor s One 
    (PMeld s ts)       -> meldHelper2 PMeld s (dora . minimum $ ts) 3
    (KMeld s ts)       -> meldHelper2 KMeld s (dora . minimum $ ts) 4
    (EMeld s ts)       -> meldHelper2 EMeld s (dora . minimum $ ts) 2

