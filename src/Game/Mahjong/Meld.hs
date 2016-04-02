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
  mkChow, mkPung, mkKong, mkEyes, mkMeld,

  -- ** Meld accessors
  status, meldType, meldTiles,

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
  = Meld {
      status :: Status
    , meldType :: MeldType
    , meldTiles :: [Tile]
    }
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
  pp (Meld s mt ts) =
    pp s ++ enclose mt (joinPP " " ts)
    where
      enclose :: MeldType -> String -> String
      enclose mt ts =
        case mt of
          Chow -> "<" ++ ts ++ ">"
          Pung -> "[" ++ ts ++ "]"
          Kong -> "{" ++ ts ++ "}"
          Eyes -> "(" ++ ts ++ ")"


-------------------------------------------------------------------------------
-- Meld generation
-------------------------------------------------------------------------------

-- | Given a tile, takes next 2 successors to make a chow.
--   Only Tile one to seven of the 3 suit types are valid input,
--   suit tile of value 8 or 9, and non suit tiles returns Nothing.
mkChow :: Status -> Tile -> Maybe Meld
mkChow s t =
  if isSuit t && not (isEightOrNine t)
  then Just $ Meld s Chow $ take 3 $ iterate next t
  else Nothing

-- | Given a tile, attemps to make a Pung.
--   Bonus tile results in Nothing.
mkPung :: Status -> Tile -> Maybe Meld
mkPung s t = meldHelper s Pung t 3

-- | Given a tile, attemps to make a Kong.
--   Bonus tile results in Nothing.
mkKong :: Status -> Tile -> Maybe Meld
mkKong s t = meldHelper s Kong t 4

-- | Given a tile, attemp to make a pair of Eyes.
--   Bonus tile results in Nothing.
mkEyes :: Status -> Tile -> Maybe Meld
mkEyes s t = meldHelper s Eyes t 2

meldHelper :: Status -> MeldType -> Tile -> Int -> Maybe Meld
meldHelper s mt t n
  | not $ isBonus t = Just $ Meld s mt $ replicate n t
  | otherwise       = Nothing

-- | Attemps to create a meld with parameters given.
mkMeld :: Status -> MeldType -> Tile -> Maybe Meld
mkMeld s mt t =
  case mt of
    Chow -> mkChow s t
    Pung -> mkPung s t
    Kong -> mkKong s t
    Eyes -> mkEyes s t


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
isEyes = (== Eyes) . meldType


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
  next (Meld s mt ts) = Meld s mt $ map next ts

  prev (Meld s mt ts) = Meld s mt $ map prev ts

