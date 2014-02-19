module Game.MJ.Base (
       IValues
     , WValues
     , DValues
     , FValues
     , SValues
     , AValues
     , Tile
     , Meld
     , Pair
     , Hand
     )

where

data IValues = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
             deriving (Eq, Ord, Enum)

instance Show IValues where
  show = showIValue

showIValue :: IValues -> String
showIValue One = show 1
showIValue Two = show 2
showIValue Three = show 3
showIValue Four = show 4
showIValue Five = show 5
showIValue Six = show 6
showIValue Seven = show 7
showIValue Eight = show 8
showIValue Nine = show 9

data WValues = East | South | West | North
             deriving (Show, Eq, Ord)

data DValues = Red | Green | White
             deriving (Show, Eq, Ord)

data FValues = PlumBlossom | Orchid | Chrysanthenum | BambooTree
             deriving (Show, Eq, Ord)

data SValues = Spring | Summer | Autumn | Fall
             deriving (Show, Eq, Ord)

data AValues = Cat | Rat | Cockerel | Centipede
             deriving (Show, Eq, Ord)

data Tile = Coin IValues
          | Bamboo IValues
          | Character IValues
          | Wind WValues
          | Dragon DValues
          | Flower FValues
          | Season SValues
          | Animal AValues
          deriving (Eq, Ord)

instance Show Tile where
  show = showTile

showTile :: Tile -> String
showTile (Coin i)   = show i ++ " Coin"
showTile (Bamboo i) = show i ++ " Bamboo"
showTile (Character i) = show i ++ " Character"
showTile (Wind i) = show i
showTile (Dragon i) = show i ++ " Dragon"
showTile (Flower i) = show i
showTile (Season i) = show i
showTile (Animal i) = show i

data Meld = Chow Tile Tile Tile
          | Pung Tile Tile Tile
          | Kong Tile Tile Tile Tile
          deriving (Show)

data Pair = Eye Tile Tile
          deriving (Show)

data Hand = Regular Meld Meld Meld Meld Pair
          | SevenPair Pair Pair Pair Pair Pair Pair Pair
          | ThirteenOrphan
          deriving (Show)
