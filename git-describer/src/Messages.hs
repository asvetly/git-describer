{-# LANGUAGE OverloadedStrings #-}

module Messages
    (
      token,
      size,
      main_loop,
      getMsg,
      getMsgFunction,
      get_update_id,
      get_text,
      sendMsg,
      get_chat_id
    ) where

import                Network.HTTP.Client      (newManager)
import                Network.HTTP.Client.TLS  (tlsManagerSettings)
import                Web.Telegram.API.Bot
import                Web.Telegram.API.Bot.Responses
import                Database.HDBC
import                Database.HDBC.Sqlite3
import                Data.Text
import                Data.List as List
import                Parser
import                DB


token :: Token
token = Token $ pack "bot281687116:AAHGdr5AP7_96pE-75_UoQLILZqGRzMeUkg"


size :: Maybe Int
size = Just 20


main_loop :: Connection -> IO()
main_loop conn = do
  getMsg Nothing conn
  main_loop conn


getMsg :: Maybe Int -> Connection -> IO()
getMsg id conn = do
  manager <- newManager tlsManagerSettings
  res     <- getUpdates token id size Nothing manager
  case res of
      Left e             -> putStrLn "Request failed" >> print e
      Right (Response m) ->  getMsgFunction m conn


sendMsg :: Text -> Text -> IO()
sendMsg message chatId = do
  let request = sendMessageRequest chatId message
  manager <- newManager tlsManagerSettings
  res     <- sendMessage token request manager
  case res of
      Left e             -> putStrLn "Request failed" >> print e
      Right (Response m) -> putStrLn "Message sent"


getMsgFunction :: [Update] -> Connection -> IO ()
getMsgFunction [] _   = do putStr ""
getMsgFunction m conn = do
    processing m conn
    getMsg (Just ((get_update_id m) + 1)) conn


processing :: [Update] -> Connection -> IO ()
processing [] _        = putStr ""
processing (x:xs) conn = do
    descIO <- findDesctiption conn (unpack . toLower . get_text $ x)
    sendMsg (msg descIO)(get_chat_id x)
    processing xs conn
    where
      msg dM = case dM of
        Just desc -> pack $ desc
        otherwise -> "Error 404: command not found"


get_update_id :: [Update] -> Int
get_update_id m = update_id (List.last m)


get_text :: Update -> Text
get_text x = get_text' (message x)
    where get_text'  (Just mess) = get_text'' (text mess)
          get_text'' (Just x)    = x


get_chat_id :: Update -> Text
get_chat_id x = get_chat_id' (message x)
    where
      get_chat_id' (Just mess) = pack $ show $ chat_id (chat mess)
