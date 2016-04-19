module TileTest where

import Game.Mahjong.Tile
import Test.QuickCheck


instance Arbitratry Tile where
  arbitrary = elements allTiles
