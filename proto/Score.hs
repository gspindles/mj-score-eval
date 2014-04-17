module Game.Mahjong.Score (

)

where

import Data.List (nub)
import Game.Mahjong.Tile

data MetaInfo = Conceal
              | Revealed
              | Chow
              | Pung
              | Kong
              | Eye
              | Coin
              | Bamboo
              | Character
              | Wind
              | Dragon
              | Simple
              | Terminal 

type Meld = ([Char], [Tile])
type Hand = [Meld]


-- | Predicates for determining the type of tiles

ofCoin :: [Tile] -> Bool
ofCoin = and . map isCoin

ofBamboo :: [Tile] -> Bool
ofBamboo = and . map isBamboo 

ofCharacter :: [Tile] -> Bool
ofCharacter = and . map isCharacter 

ofWind :: [Tile] -> Bool
ofWind = and . map isWind

ofDragon :: [Tile] -> Bool
ofDragon = and . map isDragon 

ofFlower :: [Tile] -> Bool
ofFlower = and . map isFlower

ofSeason :: [Tile] -> Bool
ofSeason = and . map isSeason

ofSuit :: [Tile] -> Bool
ofSuit = and . map isSuit

ofSimple :: [Tile] -> Bool
ofSimple = and . map isSimple

ofTerminal :: [Tile] -> Bool
ofTerminal = and . map isTerminal

ofHonor :: [Tile] -> Bool
ofHonor = and . map isHonor

ofEdge :: [Tile] -> Bool
ofEdge = and . map isEdge

ofGreen :: [Tile] -> Bool
ofGreen = and . map isGreen

ofRed :: [Tile] -> Bool
ofRed = and . map isRed

ofBlue :: [Tile] -> Bool
ofBlue = and . map isBlue

ofBonus :: [Tile] -> Bool
ofBonus = and . map isBonus


-- | Predicates for determining the type of meld

isChow :: [Tile] -> Bool
isChow [] = False
isChow t = t == (take 3 . iterate incTile . head t)  && len t == 3

