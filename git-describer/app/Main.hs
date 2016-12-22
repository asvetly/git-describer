{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Messages
import           Parser

main :: IO ()
main =  do
   commands <- getCommands
   putStrLn  "All commands read"
   main_loop commands
