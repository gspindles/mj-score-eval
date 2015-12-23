-- |
-- Module      :  Game.Mahjong.Meld
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Data definitions and instances for meld.
module Game.Mahjong.Meld (
  -- * Meld, Meld types, and meld status
  MeldType(..),
  Status(..),
  Meld,

  -- ** Constructors
  mkChow, mkPung, mkKong, mkEyes,

  -- ** Meld accessors
  meldType, status, meldTiles,

  -- ** Predicates for the status of meld
  isConcealed, isRevealed,

  -- ** Predicates for the type of meld
  isChow, isPung, isKong, isEyes
) where

import Game.Mahjong.Class
import Game.Mahjong.Tile


-------------------------------------------------------------------------------
-- Data definitions
-------------------------------------------------------------------------------

-- | R: Revealed, H: Concealed
data Status
  = Revealed
  | Concealed
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | Meld types
data MeldType
  = Chow
  | Pung
  | Kong
  | Eyes
    deriving (Eq, Show)

-- | Meld data
data Meld
  = CMeld { status :: Status, meldTiles :: [Tile] }
  | PMeld { status :: Status, meldTiles :: [Tile] }
  | KMeld { status :: Status, meldTiles :: [Tile] }
  | EMeld { status :: Status, meldTiles :: [Tile] }
    deriving (Eq, Show)


-------------------------------------------------------------------------------
-- Typeclass instances
-------------------------------------------------------------------------------

instance Pretty Status where
  pp Revealed  = "+"
  pp Concealed = "-"

-- | Chow ends with >,
-- Pung ends with ],
-- Kong ends with },
-- Eye ends with ).
instance Pretty Meld where
  pp (CMeld s ts) = pp s ++ joinPP " " ts ++ ">"
  pp (PMeld s ts) = pp s ++ joinPP " " ts ++ "]"
  pp (KMeld s ts) = pp s ++ joinPP " " ts ++ "}"
  pp (EMeld s ts) = pp s ++ joinPP " " ts ++ ")"


-------------------------------------------------------------------------------
-- Meld generation
-------------------------------------------------------------------------------

-- | Given a tile, takes next 2 successors to make a chow.
--   Only Tile one to seven of the 3 suit types are valid input,
--   suit tile of value 8 or 9, and non suit tiles returns Nothing.
mkChow :: Status -> Tile -> Maybe Meld
mkChow s t =
  if isSuit t && not (isEightOrNine t)
  then Just $ CMeld s $ take 3 $ iterate next t
  else Nothing

-- | Given a tile, attemps to make a Pung.
--   Bonus tile results in Nothing.
mkPung :: Status -> Tile -> Maybe Meld
mkPung s t = meldHelper PMeld s t 3

-- | Given a tile, attemps to make a Kong.
--   Bonus tile results in Nothing.
mkKong :: Status -> Tile -> Maybe Meld
mkKong s t = meldHelper PMeld s t 4

-- | Given a tile, attemp to make a pair of Eyes.
--   Bonus tile results in Nothing.
mkEyes :: Status -> Tile -> Maybe Meld
mkEyes s t = meldHelper PMeld s t 2

meldHelper :: (Status -> [Tile] -> Meld)
            -> Status -> Tile -> Int
            -> Maybe Meld
meldHelper ctor s t n =
  if not $ isBonus t
  then Just $ ctor s $ replicate n t
  else Nothing


-------------------------------------------------------------------------------
-- Predicates for status
-------------------------------------------------------------------------------

-- | Is the meld concealed?
isConcealed :: Meld -> Bool
isConcealed = (==) Concealed . status

-- | Is the meld revealed?
isRevealed :: Meld -> Bool
isRevealed  = (==) Revealed  . status


-------------------------------------------------------------------------------
-- Predicates for meld types
-------------------------------------------------------------------------------

-- | Is the meld a chow?
isChow :: Meld -> Bool
isChow = (== Chow) . meldType

-- | Is the meld a pung?
--   kong does get counted as pung.
isPung :: Meld -> Bool
isPung = allCond [(== Pung), (== Kong)] . meldType

-- | Is the meld a kong?
isKong :: Meld -> Bool
isKong = (== Kong) . meldType

-- | Is the meld a pair of eyes?
isEyes :: Meld -> Bool
isEyes = (== Chow) . meldType


-------------------------------------------------------------------------------
-- Utilities functions
-------------------------------------------------------------------------------

-- | Gets the type of meld.
meldType :: Meld -> MeldType
meldType (CMeld _ _) = Chow
meldType (PMeld _ _) = Pung
meldType (KMeld _ _) = Kong
meldType (EMeld _ _) = Eyes


-------------------------------------------------------------------------------
-- Class Instances
-------------------------------------------------------------------------------

-- | Instance for TilePred
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

instance Loop Meld where
  next m =
    case m of
      (CMeld s ts) -> loopHelper CMeld s ts next
      (PMeld s ts) -> loopHelper PMeld s ts next
      (KMeld s ts) -> loopHelper KMeld s ts next
      (EMeld s ts) -> loopHelper EMeld s ts next

  prev m =
    case m of
      (CMeld s ts) -> loopHelper CMeld s ts prev
      (PMeld s ts) -> loopHelper PMeld s ts prev
      (KMeld s ts) -> loopHelper KMeld s ts prev
      (EMeld s ts) -> loopHelper EMeld s ts prev

loopHelper :: (Status -> [Tile] -> Meld)
           -> Status -> [Tile] -> (Tile -> Tile)
           -> Meld
loopHelper ctor s ts f = ctor s $ map f ts

