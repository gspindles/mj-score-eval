module Game.MJ.Base (
       Values
     , WTiles
     , DTiles
     , FTiles
     , STiles
     , ATiles
     , Tile
     , Meld
     , Pair
     , Hand
     )

where

import Data.List (intersperse)
import System.Random
import Data.Map (Map, insert, (!), elems, singleton)

data Values = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
             deriving (Eq, Ord, Enum)

instance Show Values where
  show = showValue

showValue :: Values -> String
showValue One   = show 1
showValue Two   = show 2
showValue Three = show 3
showValue Four  = show 4
showValue Five  = show 5
showValue Six   = show 6
showValue Seven = show 7
showValue Eight = show 8
showValue Nine  = show 9

data WTiles = East | South | West | North
             deriving (Show, Eq, Ord)

data DTiles = Red | Green | White
             deriving (Show, Eq, Ord)

data FTiles = PlumBlossom | Orchid | Chrysanthenum | BambooTree
             deriving (Show, Eq, Ord)

data STiles = Spring | Summer | Autumn | Winter
             deriving (Show, Eq, Ord)

data ATiles = Cat | Rat | Cockerel | Centipede
             deriving (Show, Eq, Ord)

data Tile = Coin Values
          | Bamboo Values
          | Character Values
          | Wind WTiles
          | Dragon DTiles
          | Flower FTiles
          | Season STiles
          | Animal ATiles
          deriving (Eq, Ord)


instance Show Tile where
  show = showTile

showTile :: Tile -> String
showTile (Coin i)   = "Coin " ++ show i
showTile (Bamboo i) = "Bamboo " ++ show i
showTile (Character i) = "Character " ++ show i
showTile (Wind i) = show i
showTile (Dragon i) = show i
showTile (Flower i) = show i
showTile (Season i) = show i
showTile (Animal i) = show i

regularTiles :: [Tile]
regularTiles = [
    Coin One, Coin Two, Coin Three, Coin Four, Coin Five
  , Coin Six, Coin Seven, Coin Eight, Coin Nine
  , Bamboo One, Bamboo Two, Bamboo Three, Bamboo Four, Bamboo Five
  , Bamboo Six, Bamboo Seven, Bamboo Eight, Bamboo Nine
  , Character One, Character Two, Character Three, Character Four, Character Five
  , Character Six, Character Seven, Character Eight, Character Nine
  , Wind East, Wind South, Wind West, Wind North
  , Dragon Red, Dragon Green, Dragon White
  ]

bonusTiles :: [Tile]
bonusTiles = [
    Flower PlumBlossom, Flower Orchid, Flower Chrysanthenum, Flower BambooTree
  , Season Spring, Season Summer, Season Autumn, Season Winter
  , Animal Cat, Animal Rat, Animal Cockerel, Animal Centipede
  ]

allTiles :: [Tile]
allTiles = regularTiles ++ bonusTiles

mjset :: [Tile]
mjset = (concat . map (take 4 . repeat) $ regularTiles) ++ bonusTiles

wall :: [Tile]
wall = fst $ fisherYates (mkStdGen 148) mjset

isCoin :: Tile -> Bool
isCoin (Coin _) = True
isCoin _        = False

isBamboo :: Tile -> Bool
isBamboo (Bamboo _) = True
isBamboo _          = False

isCharacter :: Tile -> Bool
isCharacter (Character _) = True
isCharacter _             = False

isSuit :: Tile -> Bool
isSuit = or . zipWith id [isCoin, isBamboo, isCharacter] . repeat

isTerminal :: Tile -> Bool
isTerminal (Coin a)      = elem a [One, Nine]
isTerminal (Bamboo a)    = elem a [One, Nine]
isTerminal (Character a) = elem a [One, Nine]
isTerminal _             = False

isSimple :: Tile -> Bool
isSimple (Coin a)      = not $ elem a [One, Nine]
isSimple (Bamboo a)    = not $ elem a [One, Nine]
isSimple (Character a) = not $ elem a [One, Nine]
isSimple _             = False

isWind :: Tile -> Bool
isWind (Wind _) = True
isWind _        = False

isDragon :: Tile -> Bool
isDragon (Dragon _) = True
isDragon _          = False

isHonor :: Tile -> Bool
isHonor = or . zipWith id [isWind, isDragon] . repeat

isEdge :: Tile -> Bool
isEdge = or . zipWith id [isTerminal, isHonor] . repeat

isFlower :: Tile -> Bool
isFlower (Flower _) = True
isFlower _          = False

isSeason :: Tile -> Bool
isSeason (Season _) = True
isSeason _          = False

isAnimal :: Tile -> Bool
isAnimal (Animal _) = True
isAnimal _          = False

isBonus :: Tile -> Bool
isBonus = or . zipWith id [isFlower, isSeason, isAnimal] . repeat

data Meld = Chow Tile Tile Tile
          | Pung Tile Tile Tile
          | Kong Tile Tile Tile Tile

instance Show Meld where
  show = showMeld

-- for now, assumes the melds are already valid
showMeld :: Meld -> String
showMeld (Chow (Coin a) (Coin b) (Coin c)) =
  "Chow " ++ (showMeldHelper [a, b, c]) ++ " Coins"
showMeld (Chow (Bamboo a) (Bamboo b) (Bamboo c)) =
  "Chow " ++ (showMeldHelper [a, b, c]) ++ " Bamboos"
showMeld (Chow (Character a) (Character b) (Character c)) =
  "Chow " ++ (showMeldHelper [a, b, c]) ++ " Characters"
showMeld (Chow _ _ _) =
  error "can't chow accross suit, honor tiles, nor bonus tiles"
showMeld (Pung (Coin a) _ _) =
  "Pung " ++ (showMeldHelper . take 3 . repeat $ a) ++ " Coins"
showMeld (Pung (Bamboo a) _ _) =
  "Pung " ++ (showMeldHelper . take 3 . repeat $ a) ++ " Bamboos"
showMeld (Pung (Character a) _ _) =
  "Pung " ++ (showMeldHelper . take 3 . repeat $ a) ++ " Characters"
showMeld (Pung (Wind a) _ _) =
  "Pung " ++ (showMeldHelper . take 3 . repeat $ a) ++ " Winds"
showMeld (Pung (Dragon a) _ _) =
  "Pung " ++ (showMeldHelper . take 3 . repeat $ a) ++ " Dragons"
showMeld (Pung _ _ _) =
  error "can't pung bonus tiles"
showMeld (Kong (Coin a) _ _ _) =
  "Kong " ++ (showMeldHelper . take 4 . repeat $ a) ++ " Coins"
showMeld (Kong (Bamboo a) _ _ _) =
  "Kong " ++ (showMeldHelper . take 4 . repeat $ a) ++ " Bamboos"
showMeld (Kong (Character a) _ _ _) =
  "Kong " ++ (showMeldHelper . take 4 . repeat $ a) ++ " Characters"
showMeld (Kong (Wind a) _ _ _) =
  "Kong " ++ (showMeldHelper . take 4 . repeat $ a) ++ " Winds"
showMeld (Kong (Dragon a) _ _ _) =
  "Kong " ++ (showMeldHelper . take 4 . repeat $ a) ++ " Dragons"
showMeld (Kong (Flower a) _ _ _) =
  "Kong Flowers"
showMeld (Kong (Season a) _ _ _) =
  "Kong Seasons"
showMeld (Kong (Animal a) _ _ _) =
  "Kong Animals"

showMeldHelper :: Show a => [a] -> String
showMeldHelper = concat . intersperse " " . map show

data Pair = Eye Tile Tile
          deriving (Show)

data Hand = Regular Meld Meld Meld Meld Pair
          | SevenPair Pair Pair Pair Pair Pair Pair Pair
          | ThirteenOrphan [Tile]
          deriving (Show)

-- Source:  http://www.haskell.org/haskellwiki/Random_shuffle
fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

-- Source:  http://www.haskell.org/haskellwiki/Random_shuffle
fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l =
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1..]
    initial x gen = (singleton 0 x, gen)