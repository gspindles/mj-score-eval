module Game.Mahjong.Score (

)

where

import Data.List (nub)
import Game.Mahjong.Tile


type Meld = ([Char], [Tile])
type Hand = [Meld]


-- | Predicates for determining the type of tiles

isCoin :: Meld -> Bool
isCoin [] = []
isCoin = elem 'C' . fst

isBamboo :: Meld -> Bool
isBamboo = elem 'B' . fst 

isCharacter :: Meld -> Bool
isCharacter = elem 'K' . fst 

isWind :: Meld -> Bool
isWind = elem 'W' . fst 

isDragon :: Meld -> Bool
isDragon = elem 'D' . fst 

isBonus :: Meld -> Bool
isBonus = elem 'B' . fst 

isSuit :: Meld -> Bool
isSuit = or . zipWith id [isCoin, isBamboo, isCharacter] . repeat . fst 

isHonor :: Meld -> Bool
isHonor = or . zipWith id [isWind, isDragon] . repeat . fst

isSimple :: Meld -> Bool
isSimple = elem 'S' . fst 

isTerminal :: Meld -> Bool
isTerminal = elem 'T' . fst 

isEdge :: Meld -> Bool
isEdge = or . zipWith id [isTerminal, isHonor] . repeat . fst 


-- | Predicates for determining the type of meld

isChow :: Meld -> Bool
isChow = elem 'c' . fst
--isChow t = t == (take 3 . iterate incTile . head t)  && len t == 3

isPung :: Meld -> Bool
isPung = or . zipWith id [elem 'p', elem 'k'] . repeat . fst

isKong :: Meld -> Bool
isKong [] = False
isKong = elem 'k' . fst

isEye :: Meld -> Bool
isEye [] = False
isEye = elem 'e' . fst


