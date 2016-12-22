{-# LANGUAGE OverloadedStrings #-}

module Messages
    (
      token,
      size,
      main_loop,
      getMsg,
      getMsgFunction,
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
import			 Processing

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
    processing_updates m cmds
    getMsg next_id cmds
 	where next_id = Just ((update_id (List.last m)) + 1)

processing_updates :: [Update] -> [Command] -> IO ()
processing_updates [] _        = putStr ""
processing_updates (x:xs) cmds = do
    sendMsg (get_message x cmds) (get_chat_id x)
    processing_updates xs cmds


get_chat_id :: Update -> Text
get_chat_id x = get_chat_id' (message x)
    where get_chat_id' (Just mess) = pack $ show $ chat_id (chat mess)
