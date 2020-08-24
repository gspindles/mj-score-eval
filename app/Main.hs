module Main where

import Game.Mahjong.Wall

main :: IO ()
main = do
  wall <- randomWall Include
  print wall
