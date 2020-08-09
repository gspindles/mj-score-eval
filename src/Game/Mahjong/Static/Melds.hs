-- | Data definitions and instances for meld.
module Game.Mahjong.Meld (
  -- * Meld, Meld types, and meld status
  MeldType(..),
  Status(..),
  Meld,

  -- ** Constructors
  mkChow, mkPung, mkKong, mkEyes, mkMeld, promotePung,

  -- ** Meld accessors
  status, meldType, meldTiles,

  -- ** Predicates for the status of meld
  isConcealed, isRevealed, isPromoted,

  -- ** Predicates for the type of meld
  isChow, isPung, isKong, isEyes,

  -- ** meld aliases
  c123, c234, c345, c456, c567, c678, c789,
  c111, c222, c333, c444, c555, c666, c777, c888, c999,
  c1111, c2222, c3333, c4444, c5555, c6666, c7777, c8888, c9999,
  c11, c22, c33, c44, c55, c66, c77, c88, c99,
  b123, b234, b345, b456, b567, b678, b789,
  b111, b222, b333, b444, b555, b666, b777, b888, b999,
  b1111, b2222, b3333, b4444, b5555, b6666, b7777, b8888, b9999,
  b11, b22, b33, b44, b55, b66, b77, b88, b99,
  k123, k234, k345, k456, k567, k678, k789,
  k111, k222, k333, k444, k555, k666, k777,  k888, k999,
  k1111, k2222, k3333, k4444, k5555, k6666, k7777, k8888, k9999,
  k11, k22, k33, k44, k55, k66, k77, k88, k99,
  weee, wsss, wwww, wnnn,
  weeee, wssss, wwwww, wnnnn,
  wee, wss, www, wnn,
  drrr, dggg, dwww,
  drrrr, dgggg, dwwww,
  drr, dgg, dww,

  -- ** meld collections
  coinChows, bambooChows, characterChows, terminalChows,
  coinPungs, bambooPungs, characterPungs, terminalPungs, windPungs, dragonPungs,
  coinKongs, bambooKongs, characterKongs, terminalKongs, windKongs, dragonKongs,
  coinEyes, bambooEyes, characterEyes, terminalEyes, windEyes, dragonEyes,

  -- ** Utility function
  meldTileMatch
) where

import Game.Mahjong.Class
import Game.Mahjong.Tile

import Data.List (nub, sort)

-------------------------------------------------------------------------------
-- Data definitions
-------------------------------------------------------------------------------

-- | R: Revealed, H: Concealed
data Status
  = Revealed
  | Concealed
  | Promoted
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | Meld types
data MeldType
  = Chow
  | Pung
  | Kong
  | Eyes
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
-- Chow: <>, Pung: [], Kong: {}, Eyes: ()
instance Pretty Meld where
  pp (Meld s mt ts) =
    pp s ++ enclose
    where
      ppTiles = joinPP " " ts
      enclose =
        case mt of
          Chow -> "<" ++ ppTiles ++ ">"
          Pung -> "[" ++ ppTiles ++ "]"
          Kong -> "{" ++ ppTiles ++ "}"
          Eyes -> "(" ++ ppTiles ++ ")"

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


-------------------------------------------------------------------------------
-- Meld generation
-------------------------------------------------------------------------------

-- | Tries to make a Chow.
mkChow :: Status -> [Tile] -> Maybe Meld
mkChow s ts
  | s /= Promoted && sequenceOf3 && areSuitTiles && inSequence
      = Just $ Meld s Chow ordered
  | otherwise
      =  Nothing
  where
    ordered      = sort ts
    sequenceOf3  = length ts == 3
    areSuitTiles = all isSuit ts
    inSequence   = next (ordered !! 0) == ordered !! 1
                && next (ordered !! 1) == ordered !! 2

-- | Tries to make a Pung.
mkPung :: Status -> [Tile] -> Maybe Meld
mkPung s ts
  | s /= Promoted && length ts == 3 = meldHelper s Pung ts
  | otherwise                      = Nothing

-- | Tries to make a Kong.
mkKong :: Status -> [Tile] -> Maybe Meld
mkKong s ts
  | length ts == 4 = meldHelper s Kong ts
  | otherwise      = Nothing

-- | Tries to make an Eyes.
mkEyes :: Status -> [Tile] -> Maybe Meld
mkEyes s ts
  | s /= Promoted && length ts == 2 = meldHelper s Eyes ts
  | otherwise                      = Nothing

-- | Tries to make a meld.
mkMeld :: Status -> MeldType -> [Tile] -> Maybe Meld
mkMeld s mt t =
  case mt of
    Chow -> mkChow s t
    Pung -> mkPung s t
    Kong -> mkKong s t
    Eyes -> mkEyes s t

-- | Tries to promote a Pung to a Kong.
promotePung :: Meld -> Tile -> Maybe Meld
promotePung (Meld Revealed Pung ts) t
  | all (== t) ts = Just $ Meld Promoted Kong (t:ts)
  | otherwise     = Nothing
promotePung _ _   = Nothing

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

-- | Is the meld promoted to Kong?
isPromoted :: Meld -> Bool
isPromoted  = (==) Promoted  . status

-------------------------------------------------------------------------------
-- Predicates for meld types
-------------------------------------------------------------------------------

-- | Is the meld a chow?
isChow :: Meld -> Bool
isChow = (== Chow) . meldType

-- | Is the meld a pung?
--   kong does get counted as pung.
isPung :: Meld -> Bool
isPung = anyCond [(== Pung), (== Kong)] . meldType

-- | Is the meld a kong?
isKong :: Meld -> Bool
isKong = (== Kong) . meldType

-- | Is the meld a pair of eyes?
isEyes :: Meld -> Bool
isEyes = (== Eyes) . meldType


-------------------------------------------------------------------------------
-- Meld collections
-------------------------------------------------------------------------------

{- Coins -}

c123, c234, c345, c456, c567, c678, c789 :: Meld
c123 = Meld Revealed Chow [c1, c2, c3]
c234 = Meld Revealed Chow [c2, c3, c4]
c345 = Meld Revealed Chow [c3, c4, c5]
c456 = Meld Revealed Chow [c4, c5, c6]
c567 = Meld Revealed Chow [c5, c6, c7]
c678 = Meld Revealed Chow [c6, c7, c8]
c789 = Meld Revealed Chow [c7, c8, c9]

c111, c222, c333, c444, c555, c666, c777, c888, c999 :: Meld
c111 = Meld Revealed Pung [c1, c1, c1]
c222 = Meld Revealed Pung [c2, c2, c2]
c333 = Meld Revealed Pung [c3, c3, c3]
c444 = Meld Revealed Pung [c4, c4, c4]
c555 = Meld Revealed Pung [c5, c5, c5]
c666 = Meld Revealed Pung [c6, c6, c6]
c777 = Meld Revealed Pung [c7, c7, c7]
c888 = Meld Revealed Pung [c8, c8, c8]
c999 = Meld Revealed Pung [c9, c9, c9]

c1111, c2222, c3333, c4444, c5555, c6666, c7777, c8888, c9999 :: Meld
c1111 = Meld Revealed Kong [c1, c1, c1, c1]
c2222 = Meld Revealed Kong [c2, c2, c2, c2]
c3333 = Meld Revealed Kong [c3, c3, c3, c3]
c4444 = Meld Revealed Kong [c4, c4, c4, c4]
c5555 = Meld Revealed Kong [c5, c5, c5, c5]
c6666 = Meld Revealed Kong [c6, c6, c6, c6]
c7777 = Meld Revealed Kong [c7, c7, c7, c7]
c8888 = Meld Revealed Kong [c8, c8, c8, c8]
c9999 = Meld Revealed Kong [c9, c9, c9, c9]

c11, c22, c33, c44, c55, c66, c77, c88, c99 :: Meld
c11 = Meld Revealed Eyes [c1, c1]
c22 = Meld Revealed Eyes [c2, c2]
c33 = Meld Revealed Eyes [c3, c3]
c44 = Meld Revealed Eyes [c4, c4]
c55 = Meld Revealed Eyes [c5, c5]
c66 = Meld Revealed Eyes [c6, c6]
c77 = Meld Revealed Eyes [c7, c7]
c88 = Meld Revealed Eyes [c8, c8]
c99 = Meld Revealed Eyes [c9, c9]

{- Bamboos -}

b123, b234, b345, b456, b567, b678, b789 :: Meld
b123 = Meld Revealed Chow [b1, b2, b3]
b234 = Meld Revealed Chow [b2, b3, b4]
b345 = Meld Revealed Chow [b3, b4, b5]
b456 = Meld Revealed Chow [b4, b5, b6]
b567 = Meld Revealed Chow [b5, b6, b7]
b678 = Meld Revealed Chow [b6, b7, b8]
b789 = Meld Revealed Chow [b7, b8, b9]

b111, b222, b333, b444, b555, b666, b777, b888, b999 :: Meld
b111 = Meld Revealed Pung [b1, b1, b1]
b222 = Meld Revealed Pung [b2, b2, b2]
b333 = Meld Revealed Pung [b3, b3, b3]
b444 = Meld Revealed Pung [b4, b4, b4]
b555 = Meld Revealed Pung [b5, b5, b5]
b666 = Meld Revealed Pung [b6, b6, b6]
b777 = Meld Revealed Pung [b7, b7, b7]
b888 = Meld Revealed Pung [b8, b8, b8]
b999 = Meld Revealed Pung [b9, b9, b9]

b1111, b2222, b3333, b4444, b5555, b6666, b7777, b8888, b9999 :: Meld
b1111 = Meld Revealed Kong [b1, b1, b1, b1]
b2222 = Meld Revealed Kong [b2, b2, b2, b2]
b3333 = Meld Revealed Kong [b3, b3, b3, b3]
b4444 = Meld Revealed Kong [b4, b4, b4, b4]
b5555 = Meld Revealed Kong [b5, b5, b5, b5]
b6666 = Meld Revealed Kong [b6, b6, b6, b6]
b7777 = Meld Revealed Kong [b7, b7, b7, b7]
b8888 = Meld Revealed Kong [b8, b8, b8, b8]
b9999 = Meld Revealed Kong [b9, b9, b9, b9]

b11, b22, b33, b44, b55, b66, b77, b88, b99 :: Meld
b11 = Meld Revealed Eyes [b1, b1]
b22 = Meld Revealed Eyes [b2, b2]
b33 = Meld Revealed Eyes [b3, b3]
b44 = Meld Revealed Eyes [b4, b4]
b55 = Meld Revealed Eyes [b5, b5]
b66 = Meld Revealed Eyes [b6, b6]
b77 = Meld Revealed Eyes [b7, b7]
b88 = Meld Revealed Eyes [b8, b8]
b99 = Meld Revealed Eyes [b9, b9]

{- Characters -}

k123, k234, k345, k456, k567, k678, k789 :: Meld
k123 = Meld Revealed Chow [k1, k2, k3]
k234 = Meld Revealed Chow [k2, k3, k4]
k345 = Meld Revealed Chow [k3, k4, k5]
k456 = Meld Revealed Chow [k4, k5, k6]
k567 = Meld Revealed Chow [k5, k6, k7]
k678 = Meld Revealed Chow [k6, k7, k8]
k789 = Meld Revealed Chow [k7, k8, k9]

k111, k222, k333, k444, k555, k666, k777,  k888, k999 :: Meld
k111 = Meld Revealed Pung [k1, k1, k1]
k222 = Meld Revealed Pung [k2, k2, k2]
k333 = Meld Revealed Pung [k3, k3, k3]
k444 = Meld Revealed Pung [k4, k4, k4]
k555 = Meld Revealed Pung [k5, k5, k5]
k666 = Meld Revealed Pung [k6, k6, k6]
k777 = Meld Revealed Pung [k7, k7, k7]
k888 = Meld Revealed Pung [k8, k8, k8]
k999 = Meld Revealed Pung [k9, k9, k9]

k1111, k2222, k3333, k4444, k5555, k6666, k7777, k8888, k9999 :: Meld
k1111 = Meld Revealed Kong [k1, k1, k1, k1]
k2222 = Meld Revealed Kong [k2, k2, k2, k2]
k3333 = Meld Revealed Kong [k3, k3, k3, k3]
k4444 = Meld Revealed Kong [k4, k4, k4, k4]
k5555 = Meld Revealed Kong [k5, k5, k5, k5]
k6666 = Meld Revealed Kong [k6, k6, k6, k6]
k7777 = Meld Revealed Kong [k7, k7, k7, k7]
k8888 = Meld Revealed Kong [k8, k8, k8, k8]
k9999 = Meld Revealed Kong [k9, k9, k9, k9]

k11, k22, k33, k44, k55, k66, k77, k88, k99 :: Meld
k11 = Meld Revealed Eyes [k1, k1]
k22 = Meld Revealed Eyes [k2, k2]
k33 = Meld Revealed Eyes [k3, k3]
k44 = Meld Revealed Eyes [k4, k4]
k55 = Meld Revealed Eyes [k5, k5]
k66 = Meld Revealed Eyes [k6, k6]
k77 = Meld Revealed Eyes [k7, k7]
k88 = Meld Revealed Eyes [k8, k8]
k99 = Meld Revealed Eyes [k9, k9]

{- Winds -}

weee, wsss, wwww, wnnn :: Meld
weee = Meld Revealed Pung [we, we, we]
wsss = Meld Revealed Pung [ws, ws, ws]
wwww = Meld Revealed Pung [ww, ww, ww]
wnnn = Meld Revealed Pung [wn, wn, wn]

weeee, wssss, wwwww, wnnnn :: Meld
weeee = Meld Revealed Kong [we, we, we, we]
wssss = Meld Revealed Kong [ws, ws, ws, ws]
wwwww = Meld Revealed Kong [ww, ww, ww, ww]
wnnnn = Meld Revealed Kong [wn, wn, wn, wn]

wee, wss, www, wnn :: Meld
wee = Meld Revealed Eyes [we, we]
wss = Meld Revealed Eyes [ws, ws]
www = Meld Revealed Eyes [ww, ww]
wnn = Meld Revealed Eyes [wn, wn]

{- Dragons -}

drrr, dggg, dwww :: Meld
drrr = Meld Revealed Pung [dr, dr, dr]
dggg = Meld Revealed Pung [dg, dg, dg]
dwww = Meld Revealed Pung [dw, dw, dw]

drrrr, dgggg, dwwww :: Meld
drrrr = Meld Revealed Kong [dr, dr, dr, dr]
dgggg = Meld Revealed Kong [dg, dg, dg, dg]
dwwww = Meld Revealed Kong [dw, dw, dw, dw]

drr, dgg, dww :: Meld
drr = Meld Revealed Eyes [dr, dr]
dgg = Meld Revealed Eyes [dg, dg]
dww = Meld Revealed Eyes [dw, dw]


-------------------------------------------------------------------------------
-- Meld collections
-------------------------------------------------------------------------------

{- Chows -}

coinChows :: [Meld]
coinChows = [c123, c234, c345, c456, c567, c678, c789]

bambooChows :: [Meld]
bambooChows = [b123, b234, b345, b456, b567, b678, b789]

characterChows :: [Meld]
characterChows = [k123, k234, k345, k456, k567, k678, k789]

terminalChows :: [Meld]
terminalChows = [c123, c789, b123, b789, k123, k789]

{- Pungs -}

coinPungs :: [Meld]
coinPungs = [c111, c222, c333, c444, c555, c666, c777, c888, c999]

bambooPungs :: [Meld]
bambooPungs = [b111, b222, b333, b444, b555, b666, b777, b888, b999]

characterPungs :: [Meld]
characterPungs = [k111, k222, k333, k444, k555, k666, k777, k888, k999]

terminalPungs :: [Meld]
terminalPungs = [c111, c999, b111, b999, k111, k999]

windPungs :: [Meld]
windPungs = [weee, wsss, wwww, wnnnn]

dragonPungs :: [Meld]
dragonPungs = [drrr, dggg, dwww]

{- Kongs -}

coinKongs :: [Meld]
coinKongs = [c1111, c2222, c3333, c4444, c5555, c6666, c7777, c8888, c9999]

bambooKongs :: [Meld]
bambooKongs = [b1111, b2222, b3333, b4444, b5555, b6666, b7777, b8888, b9999]

characterKongs :: [Meld]
characterKongs = [k1111, k2222, k3333, k4444, k5555, k6666, k7777, k8888, k9999]

terminalKongs :: [Meld]
terminalKongs = [c1111, c9999, b1111, b9999, k1111, k9999]

windKongs :: [Meld]
windKongs = [weeee, wssss, wwwww, wnnnn]

dragonKongs :: [Meld]
dragonKongs = [drrrr, dgggg, dwwww]

{- Eyess -}

coinEyes :: [Meld]
coinEyes = [c11, c22, c33, c44, c55, c66, c77, c88, c99]

bambooEyes :: [Meld]
bambooEyes = [b11, b22, b33, b44, b55, b66, b77, b88, b99]

characterEyes :: [Meld]
characterEyes = [k11, k22, k33, k44, k55, k66, k77, k88, k99]

terminalEyes :: [Meld]
terminalEyes = [c11, c99, b11, b99, k11, k99]

windEyes :: [Meld]
windEyes = [wee, wss, www, wnn]

dragonEyes :: [Meld]
dragonEyes = [drr, dgg, dww]


-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

-- | The bool: False takes into consideration that pung and kong are different
meldTileMatch :: Bool -> Meld -> Meld  -> Bool
meldTileMatch k m1 m2 =
  if k
  then ignoreMeldTypeEq (meldType m1) (meldType m2)
    && (nub $ meldTiles m1) == (nub $ meldTiles m2)
  else meldType m1 == meldType m2
    && meldTiles m1 == meldTiles m2
  where
    ignoreMeldTypeEq :: MeldType -> MeldType -> Bool
    ignoreMeldTypeEq Pung Kong = True
    ignoreMeldTypeEq Kong Pung = True
    ignoreMeldTypeEq mt1  mt2  = mt1 == mt2

