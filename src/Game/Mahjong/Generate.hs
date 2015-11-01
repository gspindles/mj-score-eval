-- |
-- Module      :  Game.Mahjong.Meld.Generate
-- Copyright   :  Joseph Ching 2015
-- License     :  MIT
--
-- Maintainer  :  joseph.m.ching@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- | Meta tile and meld data definition
--   along with methods for generation of particular kinds of melds
module Game.Mahjong.Meld.Generate (
) where

import Game.Mahjong.Meld
import Game.Mahjong.Tile

import Data.List (nub, unfoldr)
import System.IO.Unsafe (unsafePerformIO)
import System.Random


--------------------------------------------------------------------------------
-- Get a Tile
--------------------------------------------------------------------------------

helper :: [Tile] -> Int -> Tile
helper ts x = ts !! mod x (length ts)

getCoin :: Int -> Tile
getCoin = helper coins

getBamboo :: Int -> Tile
getBamboo = helper bamboos

getCharacter :: Int -> Tile
getCharacter = helper characters

getWind :: Int -> Tile
getWind = helper winds

getDragon :: Int -> Tile
getDragon = helper dragons

getFlower :: Int -> Tile
getFlower = helper flowers

getSeason :: Int -> Tile
getSeason = helper seasons

getAnimal :: Int -> Tile
getAnimal = helper animals

getSimple :: Int -> Tile
getSimple = helper simples

getTerminal :: Int -> Tile
getTerminal = helper terminals

getSuit :: Int -> Tile
getSuit = helper suits

getHonor :: Int -> Tile
getHonor = helper honors

getEdge :: Int -> Tile
getEdge = helper edges

getBonus :: Int -> Tile
getBonus = helper bonuses

getExtra :: Int -> Tile
getExtra = helper bonuses

getGreen :: Int -> Tile
getGreen = helper greens

getRed :: Int -> Tile
getRed = helper reds

getBlue :: Int -> Tile
getBlue = helper blues

getRandomTile :: Int -> Tile
getRandomTile = helper regulars

getTile :: AllTileType -> Int -> Tile
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

sample :: Int -> [Tile] -> [Tile]
sample size ts = nub . map (\x -> ts !! mod x (length ts)) $ (unsafePerformIO (seeds size) :: [Int])
  where
    seeds :: Int -> IO [Int]
    seeds size = do
      g <- newStdGen
      return . take size $ (randoms g :: [Int])


{- Generate the Melds -}

makeMeld :: MeldType -> Status -> AllTileType -> Int -> Either String Meld
makeMeld Chow s t x = makeChow  s $ getTile t x
makeMeld Pung s t x = makePung  s $ getTile t x
makeMeld Kong s t x = makeKong  s $ getTile t x
makeMeld Eye  s t x = makeEye   s $ getTile t x

genBonus :: Int -> [Tile]
genBonus x = sample x bonuses

genOnHand :: Int -> [Tile]
genOnHand x = sample x regulars

