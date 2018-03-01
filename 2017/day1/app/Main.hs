module Main where

import Lib

main :: IO ()
main = do
  file <- readFile "input.txt"
  print $ solveCaptcha file
