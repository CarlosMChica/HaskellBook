module Main where

import Lib
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word  <- randomWord'
  runGame $ freshPuzzle word
