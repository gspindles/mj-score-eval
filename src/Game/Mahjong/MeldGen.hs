-- |
-- Module      :  Game.Mahjong.MeldGen
-- Copyright   :  Joseph Ching 2014
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Meta tile and meld data definition
--   along with methods for generation of particular kinds of melds
module Game.Mahjong.MeldGen (
    -- Data definition
    TileType(..), MeldType(..)


  ) where

import Data.List (nub, unfoldr)
import Game.Mahjong.Meld
import Game.Mahjong.Tile
import System.IO.Unsafe (unsafePerformIO)
import System.Random

{- Data Definition -}

data TileType = CoinTile
              | BambooTile
              | CharacterTile
              | WindTile
              | DragonTile
              | FlowerTile
              | SeasonTile
              | AnimalTile
              | SimpleTile
              | TerminalTile
              | SuitTile
              | HonorTile
              | EdgeTile
              | GreenTile
              | RedTile
              | BlueTile
              | BonusTile
              | ExtraTile
                deriving (Bounded, Enum, Eq, Ord, Read, Show)

data MeldType = ChowMeld
              | PungMeld
              | KongMeld
              | EyeMeld
              | MixedMeld
              | BonusMeld
                deriving (Bounded, Enum, Eq, Ord, Read, Show)


{- Get a Tile -}

getCoin :: Int -> Tile
getCoin x = coins !! mod x 9

getBamboo :: Int -> Tile
getBamboo x = bamboos !! mod x 9

getCharacter :: Int -> Tile
getCharacter x = characters !! mod x 9

getWind :: Int -> Tile
getWind x = winds !! mod x 4

getDragon :: Int -> Tile
getDragon x = dragons !! mod x 3

getFlower :: Int -> Tile
getFlower x = flowers !! mod x 4

getSeason :: Int -> Tile
getSeason x = seasons !! mod x 4

getAnimal :: Int -> Tile
getAnimal x = animals !! mod x 4

getSimple :: Int -> Tile
getSimple x = simples !! mod x 24

getTerminal :: Int -> Tile
getTerminal x = terminals !! mod x 6

getSuit :: Int -> Tile
getSuit x = suits !! mod x 27

getHonor :: Int -> Tile
getHonor x = honors !! mod x 7

getEdge :: Int -> Tile
getEdge x = edges !! mod x 13

getBonus :: Int -> Tile
getBonus x = bonuses !! mod x 8

getExtra :: Int -> Tile
getExtra x = bonuses !! mod x 12

getGreen :: Int -> Tile
getGreen x = greens !! mod x 6

getRed :: Int -> Tile
getRed x = reds !! mod x 5

getBlue :: Int -> Tile
getBlue x = blues !! mod x 7

getTile :: TileType -> Int -> Tile
getTile CoinTile      = getCoin
getTile BambooTile    = getBamboo
getTile CharacterTile = getCharacter
getTile WindTile      = getWind
getTile DragonTile    = getDragon
getTile FlowerTile    = getFlower
getTile SeasonTile    = getSeason
getTile AnimalTile    = getAnimal
getTile SimpleTile    = getSimple
getTile TerminalTile  = getTerminal
getTile SuitTile      = getSuit
getTile HonorTile     = getHonor
getTile EdgeTile      = getEdge
getTile BonusTile     = getBonus
getTile ExtraTile     = getExtra
getTile GreenTile     = getGreen
getTile RedTile       = getRed
getTile BlueTile      = getBlue

getRandomTile :: Int -> Tile
getRandomTile x = regulars !! mod x 144

sample :: Int -> [Tile] -> [Tile]
sample size ts = nub . map (\x -> ts !! mod x (length ts)) $ (unsafePerformIO (seeds size) :: [Int])
  where seeds :: Int -> IO [Int]
        seeds size = do
          g <- newStdGen
          return . take size $ (randoms g :: [Int])

{- Generate the Melds -}

makeMeld :: MeldType -> Status -> TileType -> Int -> Meld
makeMeld ChowMeld  s t x = makeChow  s $ getTile t x
makeMeld PungMeld  s t x = makePung  s $ getTile t x
makeMeld KongMeld  s t x = makeKong  s $ getTile t x
makeMeld EyeMeld   s t x = makeEye   s $ getTile t x
makeMeld MixedMeld _ _ x = makeMixed   $ sample x regulars
makeMeld BonusMeld _ _ x = makeBonus   $ sample x bonuses
