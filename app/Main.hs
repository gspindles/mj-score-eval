module Main where

import Game.Mahjong.Tile

main :: IO ()
main = do
  wall <- randomWall
  print wall
