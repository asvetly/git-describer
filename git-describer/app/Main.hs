{-# LANGUAGE OverloadedStrings #-}

module Main where

import                Database.HDBC
import                Database.HDBC.Sqlite3
import                Messages
import                Parser
import                DB
import                Control.Monad


main :: IO ()
main =  do
  conn <- connectSqlite3 "commands.db"
  (checkDB conn) >>= (flip when) (getCommands >>= createDB conn)
  putStrLn  "All commands read"
  main_loop conn
  disconnect conn
