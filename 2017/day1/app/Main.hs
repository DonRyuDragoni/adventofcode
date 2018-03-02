module Main where

import Lib

main :: IO ()
main = do
  file <- readFile "input.txt"
  putStrLn $ show $ solveCaptcha1 file
  putStrLn $ show $ solveCaptcha2 file
