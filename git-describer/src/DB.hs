{-# LANGUAGE OverloadedStrings #-}

module DB
  (checkDB,
   convRow,
   findDesctiption,
   createDB) where

import                Database.HDBC
import                Database.HDBC.Sqlite3
import                Control.Monad
import                Parser

-- проверка наличия заполненной базы данных
checkDB :: Connection -> IO Bool
checkDB conn = do
  conn   <- connectSqlite3 "commands.db"
  tables <- getTables conn
  return . (==0) . length . filter (=="commands") $ tables


-- очевидно, в общем-то
convRow :: [SqlValue] -> String
convRow [sqlDesc] = fromSql sqlDesc

-- deprecated
-- преобразует Команду в строку-sql-запрос
-- makeInsertQuery :: Command -> String
-- makeInsertQuery cmd = "INSERT INTO commands (title, description) VALUES('" ++
--                                         title cmd       ++ "', " ++
--                                         description cmd ++ "')"


-- создает и заполняет таблицу команд
createDB :: Connection -> [Command] -> IO ()
createDB conn cmds = do
  run conn "CREATE TABLE commands (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, title TEXT, description TEXT)" []
  commit conn
  stmt <- prepare conn "INSERT INTO commands (title, description) VALUES (?, ?)"
  mapM_ (\(Command t d)-> execute stmt [toSql t, toSql d]) cmds
  commit conn


-- посик описания по названию
findDesctiption :: Connection -> String -> IO (Maybe String)
findDesctiption conn title = do
  desc <- quickQuery conn ("SELECT description FROM commands WHERE title='"
                                                          ++ title ++ "'") []
  return $ case desc of
             []     -> Nothing
             (x:xs) -> Just $ convRow x
