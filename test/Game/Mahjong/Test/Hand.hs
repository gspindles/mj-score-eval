module Game.Mahjong.Test.Hand ( tests ) where

import Game.Mahjong.Class
import Game.Mahjong.Tile
import Game.Mahjong.Static.Tiles
import Game.Mahjong.Meld
import Game.Mahjong.Static.Melds
import Game.Mahjong.Hand
import Game.Mahjong.Static.Tiles

import Data.Maybe (fromJust)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Game.Mahjong.Meld Tests" []
