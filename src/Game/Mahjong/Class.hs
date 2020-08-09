-- | Various predicates on tile types
module Game.Mahjong.Class (
  -- * Tile predicates type class
  TilePred(..),


  -- * Cycle type class
  Cycle(..),


  -- * Pretty type class
  Pretty(..),


  -- * Helper functions
  anyCond, allCond, sumCond,
  nextHelper, prevHelper,
  joinPP, showPP
) where

import Data.List (intercalate)

-------------------------------------------------------------------------------
-- TilePred typeclass
-------------------------------------------------------------------------------

class TilePred a where
  -- | Predicates for suit
  isCoin      :: a -> Bool
  isBamboo    :: a -> Bool
  isCharacter :: a -> Bool

  isSimple    :: a -> Bool
  isTerminal  :: a -> Bool
  isSuit      :: a -> Bool

  -- | Predicates for honor
  isWind      :: a -> Bool
  isDragon    :: a -> Bool

  isHonor     :: a -> Bool
  isEdge      :: a -> Bool

  -- | Predicates for bonus
  isFlower    :: a -> Bool
  isSeason    :: a -> Bool

  isBonus     :: a -> Bool

  -- | Predicates for color
  isRed       :: a -> Bool
  isGreen     :: a -> Bool


-------------------------------------------------------------------------------
-- Cycle typeclass
-------------------------------------------------------------------------------

-- | Cycle type class.
class Cycle a where
  next :: a -> a
  prev :: a -> a


-------------------------------------------------------------------------------
-- Pretty typeclass
-------------------------------------------------------------------------------

-- | Pretty type class.
class Show a => Pretty a where
  pp :: a -> String

instance Pretty Int where
  pp = show

instance Pretty a => Pretty (Maybe a) where
  pp (Just a) = pp a
  pp Nothing  = ""


-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

anyCond :: [(a -> Bool)] -> a -> Bool
anyCond fs x = or $ map ($ x) fs

allCond :: [(a -> Bool)] -> a -> Bool
allCond fs = and . zipWith id fs . repeat

sumCond :: Num n => [(a -> n)] -> a -> n
sumCond fs = sum . zipWith ($) fs . repeat

nextHelper :: (Bounded a, Enum a, Eq a) => a -> a
nextHelper a =
  if a == maxBound
  then minBound
  else succ a

prevHelper :: (Bounded a, Enum a, Eq a) => a -> a
prevHelper a =
  if a == minBound
  then maxBound
  else pred a

joinPP :: Pretty a => String -> [a] -> String
joinPP delim = intercalate delim . fmap pp

showPP :: Pretty p => p -> IO ()
showPP = putStrLn . pp
