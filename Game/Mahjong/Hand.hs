module Hand

import Tile

-- | Predicates for determining the type of tiles

ofCoin :: [Meld] -> Bool
ofCoin = and . map isCoin . fst

ofBamboo :: [Meld] -> Bool
ofBamboo = and . map isBamboo . fst

ofCharacter :: [Meld] -> Bool
ofCharacter = and . map isCharacter . fst

ofWind :: [Meld] -> Bool
ofWind = and . map isWind . fst

ofDragon :: [Meld] -> Bool
ofDragon = and . map isDragon . fst

ofFlower :: [Meld] -> Bool
ofFlower = and . map isFlowe . fstr

ofSeason :: [Meld] -> Bool
ofSeason = and . map isSeason . fst

ofSuit :: [Meld] -> Bool
ofSuit = and . map isSuit . fst

ofSimple :: [Meld] -> Bool
ofSimple = and . map isSimple . fst

ofTerminal :: [Meld] -> Bool
ofTerminal = and . map isTerminal . fst

ofHonor :: [Meld] -> Bool
ofHonor = and . map isHonor . fst

ofEdge :: [Meld] -> Bool
ofEdge = and . map isEdge . fst

ofGreen :: [Meld] -> Bool
ofGreen = and . map isGreen . fst

ofRed :: [Meld] -> Bool
ofRed = and . map isRed . fst

ofBlue :: [Meld] -> Bool
ofBlue = and . map isBlue . fst

ofBonus :: [Meld] -> Bool
ofBonus = and . map isBonus . fst
