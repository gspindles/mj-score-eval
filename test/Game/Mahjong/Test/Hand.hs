module Game.Mahjong.Test.Hand ( tests ) where

import Game.Mahjong.Class
import Game.Mahjong.Tile
import Game.Mahjong.Static.Tiles
import Game.Mahjong.Meld
import Game.Mahjong.Static.Melds
import Game.Mahjong.Hand
import Game.Mahjong.Static.Tiles

import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe (fromJust)

tests :: TestTree
tests = testGroup "Game.Mahjong.Hand Tests" []
