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
import Data.List (intercalate, sort)


-------------------------------------------------------------------------------

{- Data definitions -}

-- | R: Revealed, H: Concealed
data Status        = Revealed | Concealed
                     deriving (Bounded, Enum, Eq, Ord)

-- | Meld types
data MeldType      = Chow | Pung | Kong | Eyes
                     deriving (Eq, Show)

-- | Meld data
data Meld          = Meld { meldType  :: MeldType
                          , status    :: Status
                          , meldTiles :: Tiles
                          }


-------------------------------------------------------------------------------

{- Class instances -}

instance Show Status where
  show Revealed         = "\\"
  show Concealed        = "/"

-- | show unfinished stuff (), then revealed <>, then concealed [], then bonus {}
instance Show Meld where
  show (Meld Chow s ts) = show s ++ join' " " ts ++ ">"
  show (Meld Pung s ts) = show s ++ join' " " ts ++ "]"
  show (Meld Kong s ts) = show s ++ join' " " ts ++ "}"
  show (Meld Eyes s ts) = show s ++ join' " " ts ++ ")"

  showList ms s         = intercalate "  " . map show $ ms

join' :: Show a => String -> [a] -> String
join' delim             = intercalate delim . map show


-- | TODO: Add instance for Eq and maybe Ord


-------------------------------------------------------------------------------

{- Meld generation -}

-- | Given a suit tile, takes 2 successors and a chow
--   In the case Eight or Nine is provied, the chow will just be Seven Eight Nine
mkChow :: Status -> Tile Suit -> Meld
mkChow s t = 
  case t of
    (CTile c) -> mkHelper s c CTile
    (BTile b) -> mkHelper s b BTile
    (KTile k) -> mkHelper s k KTile
  where
    mkHelper :: Status -> Values -> (Values -> Tile Suit) -> Meld 
    mkHelper s v tCtor =
      if elem v [Eight, Nine]
      then Meld Chow s . map (mkWrap . tCtor) $ [Seven, Eight, Nine]
      else Meld Chow s . map (mkWrap . tCtor) . take 3 . iterate succ $ v

mkPung, mkKong, mkEyes :: (Pungable t) => Status -> Tile t -> Meld
mkPung s   = Meld Pung s . map mkWrap . replicate 3
mkKong s   = Meld Kong s . map mkWrap . replicate 4
mkEyes s   = Meld Eyes s . map mkWrap . replicate 2


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

{- Utilities functions -}

shiftMeld :: Meld -> Meld
shiftMeld m =
  case m of
    (Meld Chow s ts) ->
      case head . sort $ ts of
        (Wrap (CTile c)) -> chowHelper s c c1 CTile
        (Wrap (BTile b)) -> chowHelper s b b1 BTile
        (Wrap (KTile k)) -> chowHelper s k k1 KTile
      where
        chowHelper :: Status -> Values -> Tile Suit -> (Values -> Tile Suit) -> Meld
        chowHelper s v v1 tCtor =
          if v < Seven
          then mkChow s . tCtor $ succ v
          else mkChow s v1
    (Meld Pung s ts) -> shiftHelper mkPung s $ head . sort $ ts
    (Meld Kong s ts) -> shiftHelper mkKong s $ head . sort $ ts
    (Meld Eyes s ts) -> shiftHelper mkEyes s $ head . sort $ ts
  where
    shiftHelper :: (forall t. Pungable t => Status -> Tile t -> Meld)
                 -> Status
                 -> WrapTile
                 -> Meld
    shiftHelper mCtor s wt =
      case wt of
        (Wrap t@(CTile _)) -> mCtor s $ dora t
        (Wrap t@(BTile _)) -> mCtor s $ dora t
        (Wrap t@(KTile _)) -> mCtor s $ dora t
        (Wrap t@(WTile _)) -> mCtor s $ dora t
        (Wrap t@(DTile _)) -> mCtor s $ dora t

