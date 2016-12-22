module Main where

import Lib
import Parser

main :: IO ()
main = do
  commands <- getCommands
  print commands
