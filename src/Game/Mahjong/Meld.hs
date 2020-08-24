-- | Data definitions and instances for meld.
module Game.Mahjong.Meld (
  -- * Meld, Meld types, and meld status
  MeldType(..),
  Status(..),
  Meld,

  -- ** Constructors
  mkSequence, mkTriplet, mkQuartet, mkPair, mkMeld, promoteTriplet,

  -- ** Meld accessors
  status, meldType, meldTiles,

  -- ** Predicates for the status of meld
  isConcealed, isRevealed, isPromoted,

  -- ** Predicates for the type of meld
  isSequence, isTriplet, isQuartet, isPair,

  -- ** Utility function
  meldTileMatch
) where

import Game.Mahjong.Class
import Game.Mahjong.Tile

import Data.List (nub, sort)

-------------------------------------------------------------------------------
-- Data definitions
-------------------------------------------------------------------------------

-- | Meld status
data Status
  = Revealed
  | Concealed
  | Promoted
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | Meld types
data MeldType
  = Sequence
  | Triplet
  | Quartet
  | Pair
    deriving (Bounded, Enum, Eq, Ord, Show)

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
  pp Promoted  = "^"

-- | Enclose melds in different brackets
-- Sequence: <>, Triplet: [], Quartet: {}, Pair: ()
instance Pretty Meld where
  pp (Meld s mt ts) =
    pp s ++ enclose
    where
      ppTiles = joinPP " " ts
      enclose =
        case mt of
          Sequence -> "<" ++ ppTiles ++ ">"
          Triplet  -> "[" ++ ppTiles ++ "]"
          Quartet  -> "{" ++ ppTiles ++ "}"
          Pair     -> "(" ++ ppTiles ++ ")"

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

  isBonus     = any isBonus . meldTiles

  isRed       = all isRed   . meldTiles
  isGreen     = all isGreen . meldTiles

instance Cycle Meld where
  next (Meld s mt ts) = Meld s mt $ map next ts
  prev (Meld s mt ts) = Meld s mt $ map prev ts


-------------------------------------------------------------------------------
-- Meld generation
-------------------------------------------------------------------------------

-- | Tries to make a Sequence.
mkSequence :: Status -> [Tile] -> Maybe Meld
mkSequence s ts
  | s /= Promoted && sequenceOf3 && areSuitTiles && inSequence
      = Just $ Meld s Sequence ordered
  | otherwise
      =  Nothing
  where
    ordered      = sort ts
    sequenceOf3  = length ts == 3
    areSuitTiles = all isSuit ts
    inSequence   = next (ordered !! 0) == ordered !! 1
                && next (ordered !! 1) == ordered !! 2

-- | Tries to make a Triplet.
mkTriplet :: Status -> [Tile] -> Maybe Meld
mkTriplet s ts
  | s /= Promoted && length ts == 3 = meldHelper s Triplet ts
  | otherwise                      = Nothing

-- | Tries to make a Quartet.
mkQuartet :: Status -> [Tile] -> Maybe Meld
mkQuartet s ts
  | length ts == 4 = meldHelper s Quartet ts
  | otherwise      = Nothing

-- | Tries to make an Pair.
mkPair :: Status -> [Tile] -> Maybe Meld
mkPair s ts
  | s /= Promoted && length ts == 2 = meldHelper s Pair ts
  | otherwise                      = Nothing

-- | Tries to make a meld.
mkMeld :: Status -> MeldType -> [Tile] -> Maybe Meld
mkMeld s mt ts =
  case mt of
    Sequence -> mkSequence s ts
    Triplet  -> mkTriplet s ts
    Quartet  -> mkQuartet s ts
    Pair     -> mkPair s ts

-- | Tries to promote a Triplet to a Quartet.
promoteTriplet :: Meld -> Tile -> Maybe Meld
promoteTriplet (Meld Revealed Triplet ts) t
  | all (== t) ts  = Just $ Meld Promoted Quartet (t:ts)
  | otherwise      = Nothing
promoteTriplet _ _ = Nothing

meldHelper :: Status -> MeldType -> [Tile] -> Maybe Meld
meldHelper s mt ts
  | notBonusTiles && allSame = Just $ Meld s mt ts
  | otherwise                = Nothing
  where
    notBonusTiles = all (not . isBonus) ts
    allSame       = (== 1) . length . nub $ ts

-------------------------------------------------------------------------------
-- Predicates for status
-------------------------------------------------------------------------------

-- | Is the meld concealed?
isConcealed :: Meld -> Bool
isConcealed = (==) Concealed . status

-- | Is the meld revealed?
isRevealed :: Meld -> Bool
isRevealed  = (==) Revealed  . status

-- | Is the meld promoted to Quartet?
isPromoted :: Meld -> Bool
isPromoted  = (==) Promoted  . status

-------------------------------------------------------------------------------
-- Predicates for meld types
-------------------------------------------------------------------------------

-- | Is the meld a sequence?
isSequence :: Meld -> Bool
isSequence = (== Sequence) . meldType

-- | Is the meld a triplet?
--   quartet does get counted as triplet.
isTriplet :: Meld -> Bool
isTriplet = anyCond [(== Triplet), (== Quartet)] . meldType

-- | Is the meld a quartet?
isQuartet :: Meld -> Bool
isQuartet = (== Quartet) . meldType

-- | Is the meld a pair of eyes?
isPair :: Meld -> Bool
isPair = (== Pair) . meldType


-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

-- | The bool: False takes into consideration that triplet and quartet are different
meldTileMatch :: Bool -> Meld -> Meld  -> Bool
meldTileMatch k m1 m2 =
  if k
  then ignoreMeldTypeEq (meldType m1) (meldType m2)
    && (nub $ meldTiles m1) == (nub $ meldTiles m2)
  else meldType m1 == meldType m2
    && meldTiles m1 == meldTiles m2
  where
    ignoreMeldTypeEq :: MeldType -> MeldType -> Bool
    ignoreMeldTypeEq Triplet Quartet = True
    ignoreMeldTypeEq Quartet Triplet = True
    ignoreMeldTypeEq mt1     mt2     = mt1 == mt2

