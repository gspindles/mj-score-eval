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
import Data.List (intercalate, sort)


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

  showList ms s         = intercalate "  " . map show $ ms

join' :: Show a => String -> [a] -> String
join' delim             = intercalate delim . map show


-------------------------------------------------------------------------------

{- Unsafe meld generation -}

-- | Given a suit tile, takes 2 successors and a chow.
-- In the case Eight or Nine is provided, the chow will just be Seven Eight Nine.
-- Given a non suit tile, return an error message.
mkChow :: Status -> Tile -> Meld
mkChow s t              =
  case t of
    (CTile c)           -> chowHelper s c CTile
    (BTile b)           -> chowHelper s b BTile
    (KTile k)           -> chowHelper s k KTile
    _                   -> error "Can only make a chow with suit tiles."
  where
    chowHelper :: Status -> Values -> (Values -> Tile) -> Meld
    chowHelper s v tCtor =
      if elem v [Eight, Nine]
      then CMeld s . map tCtor $ [Seven, Eight, Nine]
      else CMeld s . map tCtor . take 3 . iterate succ $ v

mkPung, mkKong, mkEyes :: Status -> Tile -> Meld
mkPung s t              = meldHelper PMeld Pung s t 3
mkKong s t              = meldHelper KMeld Kong s t 4
mkEyes s t              = meldHelper EMeld Eyes s t 2

meldHelper :: (Status -> [Tile] -> Meld)
           -> MeldType -> Status -> Tile -> Int
           -> Meld
meldHelper mCtor mt s t n =
  case t of
    (CTile c)           -> mCtor s $ replicate n t
    (BTile b)           -> mCtor s $ replicate n t
    (KTile k)           -> mCtor s $ replicate n t
    (WTile w)           -> mCtor s $ replicate n t
    (DTile d)           -> mCtor s $ replicate n t
    _                   ->
      let msg = "Cannot make a " ++ show mt ++ " out of bonus tiles."
      in error msg


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
isPung = foldr (||) False . zipWith id [(== Pung), (== Kong)] . repeat . meldType
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
      case head . sort $ ts of
        (CTile c)   -> chowHelper s c c1 CTile
        (BTile b)   -> chowHelper s b b1 BTile
        (KTile k)   -> chowHelper s k k1 KTile
      where
        chowHelper :: Status -> Values -> Tile -> (Values -> Tile) -> Meld
        chowHelper s v t tCtor =
          if v < Seven
          then mkChow s . tCtor $ succ v
          else mkChow s t
    (PMeld s ts)       -> mkPung s $ dora . head . sort $ ts
    (KMeld s ts)       -> mkKong s $ dora . head . sort $ ts
    (EMeld s ts)       -> mkEyes s $ dora . head . sort $ ts

