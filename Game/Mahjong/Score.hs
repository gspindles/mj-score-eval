-- | OUTDATED
module Score (

)

where

import Data.List (nub)
import qualified Tile as T


type Meld = ([Char], [T.Tile])
type Hand = [Meld]


-- | Predicates for determining the type of tiles

isCoinMeld :: Meld -> Bool
isCoinMeld = elem 'C' . fst

isBambooMeld :: Meld -> Bool
isBambooMeld = elem 'B' . fst 

isCharacterMeld :: Meld -> Bool
isCharacterMeld = elem 'K' . fst 

isWindMeld :: Meld -> Bool
isWindMeld = elem 'W' . fst 

isDragonMeld :: Meld -> Bool
isDragonMeld = elem 'D' . fst

isBonusMeld :: Meld -> Bool
isBonusMeld = elem 'B' . fst 

isSuitMeld :: Meld -> Bool
isSuitMeld = or . zipWith id [isCoinMeld, isBambooMeld, isCharacterMeld] . repeat

isHonorMeld :: Meld -> Bool
isHonorMeld = or . zipWith id [isWindMeld, isDragonMeld] . repeat

isSimpleMeld :: Meld -> Bool
isSimpleMeld = elem 'S' . fst 

isTerminalMeld :: Meld -> Bool
isTerminalMeld = elem 'T' . fst 

isEdgeMeld :: Meld -> Bool
isEdgeMeld = or . zipWith id [isTerminalMeld, isHonorMeld] . repeat


-- | Predicates for determining the type of meld

isChowMeld :: Meld -> Bool
isChowMeld = elem 'c' . fst
--isChow t = t == (take 3 . iterate incTile . head t)  && len t == 3

isPungMeld :: Meld -> Bool
isPungMeld = or . zipWith id [elem 'p', elem 'k'] . repeat . fst

isKongMeld :: Meld -> Bool
isKongMeld = elem 'k' . fst

isEyeMeld :: Meld -> Bool
isEyeMeld = elem 'e' . fst

isConcealedMeld :: Meld -> Bool
isConcealedMeld = elem 'c' . fst

isRevealedMeld :: Meld -> Bool
isRevealedMeld = not . isConcealedMeld
