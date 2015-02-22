{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

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
import Data.List (intersperse)
 

-------------------------------------------------------------------------------

{- Data definitions -}

-- | R: Revealed, H: Concealed
data Status        = Revealed | Concealed
                     deriving (Bounded, Enum, Eq, Ord)

-- | Meld types
data MeldType      = Chow | Pung | Kong | Eyes
                     deriving (Eq, Show)

-- | Meld data
data Meld          = Meld { meldType :: MeldType
                          , status   :: Status
                          , meld     :: Tiles
                          }


-------------------------------------------------------------------------------

{- Class instances -}

instance Show Status where
  show Revealed  = "\\"
  show Concealed = "/"

-- | show unfinished stuff (), then revealed <>, then concealed [], then bonus {}
instance Show Meld where
  show (Meld Chow s ts) = show s ++ join' ts ++ ">"
  show (Meld Pung s ts) = show s ++ join' ts ++ "]"
  show (Meld Kong s ts) = show s ++ join' ts ++ "}"
  show (Meld Eyes s ts) = show s ++ join' ts ++ ")"

join' :: Tiles -> String
join' = concat . intersperse " " . map show


-------------------------------------------------------------------------------

{- Meld generation -}

-- | Given a suit tile, takes 2 successors and a chow
--   In the case Eight or Nine is provied, the chow will just be Seven Eight Nine
mkChow :: Status -> Tile Suit -> Meld
mkChow s t = 
  case t of
    (CTile c) -> mkHelper s t c CTile
    (BTile b) -> mkHelper s t b BTile
    (KTile k) -> mkHelper s t k KTile
  where
    mkHelper :: Status -> Tile Suit -> Values -> (Values -> Tile Suit) -> Meld 
    mkHelper s t v tCtor =
      if elem v [Eight, Nine]
      then Meld Chow s . map (mkWrap . tCtor) $ [Seven, Eight, Nine]
      else Meld Chow s . map mkWrap . take 3 . iterate dora $ t

mkPung, mkKong, mkEyes :: (Pungable t) => Status -> Tile t -> Meld
mkPung s = Meld Pung s . map mkWrap . replicate 3
mkKong s = Meld Kong s . map mkWrap . replicate 4
mkEyes s = Meld Eyes s . map mkWrap . replicate 2


-------------------------------------------------------------------------------

{- Predicates for determining meld types -}

isChow, isPung, isKong, isEyes :: Meld -> Bool
isChow (Meld Chow _ _) = True
isChow _               = False

-- | Kong do count as pung during scoring process
isPung (Meld Pung _ _) = True
isPung (Meld Kong _ _) = True
isPung _               = True

isKong (Meld Kong _ _) = True
isKong _               = False

isEyes (Meld Eyes _ _) = True
isEyes _               = False


-------------------------------------------------------------------------------

{- Meld Predicates with Respect to Tile Types -}

isCoinM, isBambooM, isCharacterM, isWindM, isDragonM :: Meld -> Bool
isCoinM      = predHelper isCoinT
isBambooM    = predHelper isBambooT
isCharacterM = predHelper isCharacterT
isWindM      = predHelper isWindT
isDragonM    = predHelper isDragonT

isSimpleM, isTerminalM, isSuitM, isHonorM, isEdgeM :: Meld -> Bool
isSimpleM    = and . mapWrap isSimpleT . meld
isTerminalM  = or . mapWrap isTerminalT . meld
isSuitM      = predHelper isSuitT
isHonorM     = predHelper isHonorT
isEdgeM      = predHelper isEdgeT

isRedM, isGreenM, isBlueM :: Meld -> Bool
isRedM       = predHelper isRedT
isGreenM     = predHelper isGreenT
isBlueM      = predHelper isBlueT

predHelper :: (forall a. Tile a -> Bool) -> Meld -> Bool
predHelper p = liftWrap p . head . meld

