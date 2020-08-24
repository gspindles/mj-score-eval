-- | Meta tile and meld data definition
--   along with methods for generation of particular kinds of melds
module Game.Mahjong.Generate (
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
makeMeld Sequence s t x = makeSequence s $ getTile t x
makeMeld Triplet  s t x = makeTriplet  s $ getTile t x
makeMeld Quartet  s t x = makeQuartet  s $ getTile t x
makeMeld Pair     s t x = makePair     s $ getTile t x

genBonus :: Int -> [Tile]
genBonus x = sample x bonuses

genOnHand :: Int -> [Tile]
genOnHand x = sample x regulars

