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

import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot
import           Web.Telegram.API.Bot.Responses
import           Data.Text
import           Data.List as List
import           Parser

token :: Token
token = Token $ pack "bot281687116:AAHGdr5AP7_96pE-75_UoQLILZqGRzMeUkg"

size :: Maybe Int
size = Just 20

main_loop :: [Command] -> IO()
main_loop cmds = do
   getMsg Nothing cmds
   main_loop cmds    

getMsg :: Maybe Int -> [Command] -> IO()
getMsg id cmds = do
  manager <- newManager tlsManagerSettings
  res <- getUpdates token id size Nothing manager
  case res of
    Left e -> do
      putStrLn "Request failed"
      print e
    Right (Response m) -> do
      getMsgFunction m cmds

sendMsg :: Text -> Text -> IO()
sendMsg message chatId = do
  manager <- newManager tlsManagerSettings
  let request = sendMessageRequest chatId message
  res <- sendMessage token request manager
  case res of
    Left e -> do
      putStrLn "Request failed"
      print e
    Right (Response m) -> do putStrLn "Message sent"


getMsgFunction :: [Update] -> [Command] -> IO ()
getMsgFunction [] _   = do putStr ""
getMsgFunction m cmds = do
    processing m cmds
    getMsg (Just ((get_update_id m) + 1)) cmds


processing :: [Update] -> [Command] -> IO ()
processing [] _        = putStr ""
processing (x:xs) cmds = do
    sendMsg msg (get_chat_id x)
    processing xs cmds
    where
      msg = case (descByTitle (unpack $ get_text x) cmds) of
        Just desc -> pack $ desc
        otherwise -> "Error 404: command not found"

get_update_id :: [Update] -> Int
get_update_id m = update_id (List.last m)

get_text :: Update -> Text
get_text x = get_text' (message x)
    where get_text' (Just mess) = get_text'' (text mess)
          get_text'' (Just x) = x

get_chat_id :: Update -> Text
get_chat_id x = get_chat_id' (message x)
    where get_chat_id' (Just mess) = pack $ show $ chat_id (chat mess)
